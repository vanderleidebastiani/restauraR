#' @title Select communities
#' @description Select simulated communities based on tests provided
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @aliases mergeSelection print.simRestSelect
#' @param x A object of class "simRest" or "simRestSelect" to perform communities selection (or additional selection). Or an object of class "simRestSelect" to print.
#' @param testsDet A vector with the deterministic selection criteria to be executed.
#' @param testsHie A vector with the hierarchical selection criteria to be executed.
#' @param group A vector with a parameter name to specify the simulation groups. This is only used for the hierarchical selection.
#' @param singleselection A logical argument to specify if only one simulation is selected by group (default singleselection = TRUE). This is only used for the hierarchical selection.
#' @param ... Objects of class "simRestSelect" to be concatenated. Additional arguments for respective methods.
#' @returns A list (class "simRestSelect") with the elements:
#' \item{call}{The arguments used.}
#' \item{selection$composition}{A matrix with species composition for selected communities.}
#' \item{selection$group}{A data frame with complementary information for selected sites.}
#' \item{selection$results}{A data frame with calculated parameters in each selected community.}
#' \item{selection$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{selection$thresholds}{A vector with the count of selected communities at each threshold. When simulations are merged, it is not shown.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{reference$multifunctionality}{A data frame with binary multifunctionality tests to reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' \item{supplementary$multifunctionality}{A data frame with binary multifunctionality tests to supplementary sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{checkReference}}, \code{\link{simulateCommunities}}, \code{\link{computeParameters}},
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' head(cerrado.mini$traits)
#' # Simulation
#' scenario <- simulateCommunities(trait = cerrado.mini$traits,
#'                          ava = "Available",
#'                          cwm = "BT",
#'                          rao = c("SLA", "Height", "Seed"),
#'                          rich = c(10, 15),
#'                          it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               trait = cerrado.mini$traits,
#'                     ava = "Available",
#'                     cwm = "BT",
#'                     rao = c("SLA", "Height", "Seed"),
#'                     cost = "Cost",
#'                     dens = "Density",
#'                     reference = cerrado.mini$reference,
#'                     supplementary = cerrado.mini$supplementary)
#' scenario
#' # Select communities - Deterministic selection
#' scenarioSelected <- selectCommunities(x = scenario,
#'                                       testsDet = c("CWM_BT > 6",
#'                                                 "rao > 2.5"))
#' scenarioSelected
#' # Select communities - Hierarchical selection
#' scenarioSelected <- selectCommunities(x = scenario,
#'                                       testsHie = c("CWM_BT > 6",
#'                                                 "rao > 2.5",
#'                                                 "cost == 'MIN'"))
#' scenarioSelected
#' @export
selectCommunities <- function(x, testsDet = NULL, testsHie = NULL, group = NULL, singleselection = TRUE){
  RES <- list(call = match.call())
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
    xComp <- x$simulation$composition
    xGroup <- x$simulation$group
    xMulti <- x$simulation$multifunctionality
  } else{
    xPar <- x$selection$results
    xComp <- x$selection$composition
    xGroup <- x$selection$group
    xMulti <- x$selection$multifunctionality
  }
  # Deterministic tests
  if(!is.null(testsDet)){
    # Adjust string to test
    completeString <- adjString("xPar", testsDet)
    # Evaluate test
    testsEval <- sapply(completeString, function(a) eval(parse(text=a)))
    pos <- apply(testsEval, 1, all)
    # Remove NA (set to FALSE)
    pos[is.na(pos)] <- FALSE
    # Select 
    selPar <- xPar[pos, , drop = FALSE] 
    selCom <- xComp[pos, , drop = FALSE]
    selGroup <- xGroup[pos, , drop = FALSE]
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- xMulti[pos, , drop = FALSE]
    } 
  } else{
    # Select all
    selPar <- xPar
    selCom <- xComp
    selGroup <- xGroup
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- xMulti
    }
  }
  # Hierarchical tests
  if(!is.null(testsHie)){
    # Set groups
    if(!is.null(group)){
      uniqueGroups <- unique(selPar[,group])
      nGroups <- length(uniqueGroups)
    } else{
      nGroups <- 1
    }
    # Selected positions
    selectedPos <- c()
    # For all groups
    for(i in 1:nGroups){
      # Sequence for all rows
      selPosTemp <- seq_len(nrow(selPar))
      if(!is.null(group)){
        # Filter in each group
        selPosTemp <- selPosTemp[selPar[,group] == uniqueGroups[i]]
      }
      # Filter parameters
      selParTemp <- selPar[selPosTemp, , drop = FALSE]
      # For all hierarchical tests
      for(j in 1:length(testsHie)){
        if(nrow(selParTemp)==1){
          break
        }
        multipleTests <- strsplit(testsHie[j], "&|\\|")[[1]]
        # Split test
        splitTestTemp <- strsplit(testsHie[j], "<|>|==|<=|>=|!=")[[1]]
        # Value part
        testValueTemp <- splitTestTemp[2]
        # If MIN or MAX
        if(length(multipleTests)==1 && (grepl("MAX", testValueTemp) || grepl("MIN", testValueTemp))){
          # Variable part
          testVarTemp <- splitTestTemp[1]
          # String to select the variable
          completeStringTemp <- paste0('selParTemp', '$', testVarTemp)
          # Evaluation
          testsEvalTemp <- sapply(completeStringTemp, function(a) eval(parse(text = a)))[,1]
          # If MAX
          if(grepl("MAX", testValueTemp)){
            testsEvalTemp <- testsEvalTemp == max(testsEvalTemp, na.rm = TRUE)
          }
          # If MIN
          if(grepl("MIN", testValueTemp)){
            testsEvalTemp <- testsEvalTemp == min(testsEvalTemp, na.rm = TRUE)
          }
        } else{
          # String to select the variable
          completeStringTemp <- adjString("selParTemp", testsHie[j])
          # Evaluation
          testsEvalTemp <- sapply(completeStringTemp, function(a) eval(parse(text = a)))[,1]
        }
        # Remove NA (set to FALSE)
        testsEvalTemp[is.na(testsEvalTemp)] <- FALSE
        # Filter if any TRUE in the evaluation, else try next test
        if(sum(testsEvalTemp)>0){
          selPosTemp <- selPosTemp[testsEvalTemp]
          selParTemp <- selParTemp[testsEvalTemp, , drop = FALSE]  
        } else{
          # Try next test
          if(j < length(testsHie)){
            next
          }
          # # tipo dois, para por ai
          # # if(nrow(selParTemp)>1){
          #   sampleTemp <- sample(nrow(selParTemp), size = 1)
          #   selPosTemp <- selPosTemp[sampleTemp]
          #   selParTemp <- selParTemp[sampleTemp,]
          # # }
          # break
        }
        # If last test, sample 1
        if(j == length(testsHie) && nrow(selParTemp)>1 && singleselection){
          sampleTemp <- sample(nrow(selParTemp), size = 1)
          selPosTemp <- selPosTemp[sampleTemp]
          selParTemp <- selParTemp[sampleTemp, , drop = FALSE]
        }
      }
      # Concatenate the selected positions
      selectedPos <- c(selectedPos, selPosTemp)
    }
    # Hierarchical selection
    selPar <- selPar[selectedPos, , drop = FALSE] 
    selCom <- selCom[selectedPos, , drop = FALSE]
    selGroup <- selGroup[selectedPos, , drop = FALSE]
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- selMulti[selectedPos, , drop = FALSE]
    }
  }
  # Number of selected communities
  nSel <- c(all = nrow(selCom))
  # Set results
  RES$selection$composition <- selCom
  RES$selection$group <- selGroup
  RES$selection$results <- selPar
  # Multifunctionality
  if(!is.null(xMulti)){
    RES$selection$multifunctionality <- selMulti
  }
  RES$selection$thresholds <- nSel
  if(!is.null(x$reference$composition)){
    RES$reference$composition <- x$reference$composition
  }
  if(!is.null(x$supplementary$composition)){
    RES$supplementary$composition <- x$supplementary$composition
  }
  if(!is.null(x$reference$results)){
    RES$reference$results <- x$reference$results
  }
  if(!is.null(x$supplementary$results)){
    RES$supplementary$results <- x$supplementary$results
  }
  if(!is.null(x$reference$multifunctionality)){
    RES$reference$multifunctionality <- x$reference$multifunctionality
  }
  if(!is.null(x$supplementary$multifunctionality)){
    RES$supplementary$multifunctionality <- x$supplementary$multifunctionality
  }
  class(RES) <- "simRestSelect"
  return(RES)
}
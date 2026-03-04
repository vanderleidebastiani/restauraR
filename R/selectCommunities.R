#' @title Select communities
#' @description Select simulated communities using filter-based, priority-based, or multi-site selection methods to identify optimal restoration solutions.
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @aliases mergeSelection print.simRestSelect
#' @param x A object of class "simRest" or "simRestSelect" to perform communities selection (or additional selection). Or an object of class "simRestSelect" to print.
#' @param testsFilter Character vector of logical tests for filter selection. Communities must satisfy all specified conditions to be selected.
#' @param testsPriority Character vector of logical tests for hierarchical priority selection. Tests are evaluated in order until at least one community is selected.
#' @param siteGroup Character vector specifying a parameter name to define site groups for priority selection. This is only used for the priority selection.
#' @param singleSelection Logical argument to specify if only one simulation is selected by site group (default singleSelection = FALSE). This is only used for the priority selection.
#' @param testsMultisite Character vector of logical tests for multi-site selection.
#' @param ... Objects of class "simRestSelect" to be concatenated. Additional arguments for respective methods.
#' @param object Object of class "simRestSelect" to summarise.
#' @returns A list (class "simRestSelect") with the elements:
#' \item{call}{The arguments used.}
#' \item{selection$composition}{A matrix with species composition for selected communities.}
#' \item{selection$group}{A data frame with complementary information for selected sites.}
#' \item{selection$baseline}{A matrix with with baseline species composition for simulated communities (contains all zeros when restComp is not provided.)}
#' \item{selection$results}{A data frame with calculated parameters in each selected community.}
#' \item{selection$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{selection$thresholds}{A vector with the count of selected communities at each threshold. When simulations are merged, it is not shown.}
#' \item{selection$multisite$combinations}{A binary matrix with communities combinations.}
#' \item{selection$multisite$results}{A data frame with calculated parameters in each communities combinations.}
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
#' data("cerrado")
#' head(cerrado$traits)
#' # Simulation
#' scenario <- simulateCommunities(traits = cerrado$traits,
#'                                 ava = "Available",
#'                                 maxDiver = c("SLA", "Height", "Seed"),
#'                                 constCWM = "BT",
#'                                 rich = c(10, 15),
#'                                 it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado$traits,
#'                               ava = "Available",
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               cost = "Cost",
#'                               dens = "Density",
#'                               reference = cerrado$reference)
#' scenario
#' # Select communities - Filter selection
#' scenarioSelectedFilter <- selectCommunities(x = scenario,
#'                                             testsFilter = c("CWM_BT > 5.9",
#'                                                             "rao > 0.2"))
#' scenarioSelectedFilter
#' # Select communities - Priority selection
#' scenarioSelectedPriority <- selectCommunities(x = scenario,
#'                                               testsPriority = c("CWM_BT > 5.9",
#'                                                                 "rao > 0.2",
#'                                                                 "Cost == 'MIN'"))
#' scenarioSelectedPriority
#' @export
selectCommunities <- function(x, testsFilter = NULL, testsPriority = NULL, siteGroup = NULL, singleSelection = FALSE, testsMultisite = NULL){
  RES <- list(call = match.call())
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
    xComp <- x$simulation$composition
    xGroup <- x$simulation$group
    xMulti <- x$simulation$multifunctionality
    xBase <- x$simulation$baseline
    # Multisite
    xParMultisite <- NULL
    xCombMultisite <- NULL
    # xParMultisite <- x$simulation$multisite$results
    # xCombMultisite <- x$simulation$multisite$combinations
  } else{
    xPar <- x$selection$results
    xComp <- x$selection$composition
    xGroup <- x$selection$group
    xMulti <- x$selection$multifunctionality
    xBase <- x$selection$baseline
    # Multi-site
    xParMultisite <- x$selection$multisite$results
    xCombMultisite <- x$selection$multisite$combinations
  }
  # Check testsMultisite
  if((!is.null(testsFilter) || !is.null(testsPriority)) && !is.null(testsMultisite)){
    stop("The testsMultisite argument cannot be specified together with the testsFilter or testsPriority arguments")
  }
  # Filter tests
  if(!is.null(testsFilter)){
    # Adjust string to test
    completeString <- adjString("xPar", testsFilter)
    # Evaluate test
    testsEval <- sapply(completeString, function(a) eval(parse(text=a)))
    pos <- apply(testsEval, 1, all)
    # Remove NA (set to FALSE)
    pos[is.na(pos)] <- FALSE
    # Select 
    selPar <- xPar[pos, , drop = FALSE] 
    selCom <- xComp[pos, , drop = FALSE]
    selGroup <- xGroup[pos, , drop = FALSE]
    selBase <- xBase[pos, , drop = FALSE]
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- xMulti[pos, , drop = FALSE]
    } 
  } else{
    # Select all
    selPar <- xPar
    selCom <- xComp
    selGroup <- xGroup
    selBase <- xBase
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- xMulti
    }
    if(!is.null(xParMultisite)){
      selParMultisite <- xParMultisite
    }
    if(!is.null(xCombMultisite)){
      selCombMultisite <- xCombMultisite
    }
  }
  # Priority tests
  if(!is.null(testsPriority)){
    # Set groups
    if(!is.null(siteGroup)){
      uniqueGroups <- unique(selPar[,siteGroup])
      nGroups <- length(uniqueGroups)
    } else{
      nGroups <- 1
    }
    # Selected positions
    selectedPos <- c()
    # For all siteGroups
    for(i in 1:nGroups){
      # Sequence for all rows
      selPosTemp <- seq_len(nrow(selPar))
      if(!is.null(siteGroup)){
        # Filter in each siteGroup
        selPosTemp <- selPosTemp[selPar[,siteGroup] == uniqueGroups[i]]
      }
      # Filter parameters
      selParTemp <- selPar[selPosTemp, , drop = FALSE]
      # For all priority tests
      for(j in 1:length(testsPriority)){
        if(nrow(selParTemp)==1){
          break
        }
        multipleTests <- strsplit(testsPriority[j], "&|\\|")[[1]]
        # Split test
        splitTestTemp <- strsplit(testsPriority[j], "<|>|==|<=|>=|!=")[[1]]
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
          completeStringTemp <- adjString("selParTemp", testsPriority[j])
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
          if(j < length(testsPriority)){
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
        if(j == length(testsPriority) && nrow(selParTemp)>1 && singleSelection){
          sampleTemp <- sample(nrow(selParTemp), size = 1)
          selPosTemp <- selPosTemp[sampleTemp]
          selParTemp <- selParTemp[sampleTemp, , drop = FALSE]
        }
      }
      # Concatenate the selected positions
      selectedPos <- c(selectedPos, selPosTemp)
    }
    # Priority selection
    selPar <- selPar[selectedPos, , drop = FALSE] 
    selCom <- selCom[selectedPos, , drop = FALSE]
    selGroup <- selGroup[selectedPos, , drop = FALSE]
    selBase <- selBase[selectedPos, , drop = FALSE]
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- selMulti[selectedPos, , drop = FALSE]
    }
  }
  # Multisite tests
  if(!is.null(testsMultisite)){
    if(is.null(xParMultisite) || is.null(xCombMultisite)){
      stop("The x argument must contain the multisite results")
    }
    # Selected positions
    # selectedPosMultisite <- c()
    # Sequence for all rows
    selPosMultisiteTemp <- seq_len(nrow(selParMultisite))
    # Filter multi-site parameters
    selParMultisiteTemp <- selParMultisite[selPosMultisiteTemp, , drop = FALSE]
    # For all multi-site tests
    for(k in 1:length(testsMultisite)){
      if(nrow(selParMultisiteTemp)==1){
        break
      }
      multipleTests <- strsplit(testsMultisite[k], "&|\\|")[[1]]
      # Split test
      splitTestTemp <- strsplit(testsMultisite[k], "<|>|==|<=|>=|!=")[[1]]
      # Value part
      testValueTemp <- splitTestTemp[2]
      # If MIN or MAX
      if(length(multipleTests)==1 && (grepl("MAX", testValueTemp) || grepl("MIN", testValueTemp))){
        # Variable part
        testVarTemp <- splitTestTemp[1]
        # String to select the variable
        completeStringTemp <- paste0('selParMultisiteTemp', '$', testVarTemp)
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
        completeStringTemp <- adjString("selParMultisiteTemp", testsMultisite[k])
        # Evaluation
        testsEvalTemp <- sapply(completeStringTemp, function(a) eval(parse(text = a)))[,1]
      }
      # Remove NA (set to FALSE)
      testsEvalTemp[is.na(testsEvalTemp)] <- FALSE
      # Filter if any TRUE in the evaluation, else try next test
      if(sum(testsEvalTemp)>0){
        selPosMultisiteTemp <- selPosMultisiteTemp[testsEvalTemp]
        selParMultisiteTemp <- selParMultisiteTemp[testsEvalTemp, , drop = FALSE]  
      } else{
        # Try next test
        if(k < length(testsMultisite)){
          next
        }
      }
      # If last test, sample 1
      if(k == length(testsMultisite) && nrow(selParMultisiteTemp)>1){
        # if(k == length(testsMultisite) && nrow(selParMultisiteTemp)>1 && singleSelection){
        sampleTemp <- sample(nrow(selParMultisiteTemp), size = 1)
        selPosMultisiteTemp <- selPosMultisiteTemp[sampleTemp]
        selParMultisiteTemp <- selParMultisiteTemp[sampleTemp, , drop = FALSE]
      }
    }
    # Concatenate the selected positions
    # selectedPosMultisite <- c(selectedPosMultisite, selPosMultisiteTemp)
    # }
    # selectedPosMultisite
    # Filter the multi-site results and combinations
    selParMultisite <- selParMultisite[selPosMultisiteTemp, , drop = FALSE] 
    selCombMultisite <- selCombMultisite[selPosMultisiteTemp, , drop = FALSE]
    selParMultisite
    selCombMultisite
    # Filter only 1 combination
    selCombNames <-  colnames(selCombMultisite)[as.logical(selCombMultisite[1,, drop = TRUE])]
    selCombNames
    selCombsMultisite <- which(selPar$Simulation  %in% selCombNames)
    selCombsMultisite
    # Multisite selection
    selPar <- selPar[selCombsMultisite, , drop = FALSE] 
    selCom <- selCom[selCombsMultisite, , drop = FALSE]
    selGroup <- selGroup[selCombsMultisite, , drop = FALSE]
    selBase <- selBase[selCombsMultisite, , drop = FALSE]
    # Multifunctionality
    if(!is.null(xMulti)){
      selMulti <- selMulti[selCombsMultisite, , drop = FALSE]
    }
  }
  # Number of selected communities
  nSel <- c(all = nrow(selCom))
  # Set results
  RES$selection$composition <- selCom
  RES$selection$group <- selGroup
  RES$selection$baseline <- selBase
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
  # Multi-site
  if(!is.null(x$selection$multisite$results)){
    RES$selection$multisite$results<- selParMultisite
  }
  if(!is.null(x$selection$multisite$combinations)){
    RES$selection$multisite$combinations<- selCombMultisite
  }
  class(RES) <- "simRestSelect"
  return(RES)
}
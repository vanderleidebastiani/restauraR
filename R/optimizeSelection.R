#' @title Optimize the selection of simulated communities
#' @description Calculate the functional parameters of selected simulated community sets to optimize multi-site simulation selections. This function generate all unique combination of one simulated community by restoration site and calculate multi-site indexes.
#' @encoding UTF-8
#' @param x A object of class "simRestSelect" to perform the optimization.
#' @param group A vector with a parameter name to specify the simulation groups. 
#' @param includeReference A logical argument to specify if include the reference sites on index calculations (default includeReference = TRUE).
#' @param maxComb The maximum number of simulation combinations to generate (default maxComb = 1000). All unique combinations can be generated, but if the unique combination surpasses the maximum, the function makes a sampled combination.
#' @param calcSimpsonBeta  A logical argument to specify if calculates Rao's quadratic entropy without functional distances (default calcSimpsonBeta = TRUE).
#' @param traits data frame or matrix with species traits. Traits as columns and species as rows.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist). Or a list for calculate multiples Rao.
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
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}}, ,
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
#' scenario <- simulateCommunities(traits = cerrado.mini$traits,
#'                                 restComp = cerrado.mini$restoration,
#'                                 maxDiver = c("SLA", "Height", "Seed"),
#'                                 constCWM = "BT",
#'                                 rich = c(10, 15),
#'                                 it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado.mini$traits,
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               reference = cerrado.mini$reference)
#' scenario
#' # Compute multifunctionality
#' scenario <- computeMultifunctionality(scenario,
#'                                       tests = c("CWM_BT > 5.9",
#'                                                 "rao > 0.2"))
#' scenario
#' # Select communities - Priority selection
#' scenarioSelected <- selectCommunities(x = scenario,
#'                                       testsPriority = c("simpson > 0.92",
#'                                                         "CWM_BT > 6"),
#'                                       group = "Site",
#'                                       singleselection = FALSE)
#' scenarioSelected
#' # Optimize selection
#' scenarioSelectedMultisite <- optimizeSelection(scenarioSelected,
#'                                           group = "Site",
#'                                           traits = cerrado.mini$traits,
#'                                           rao = c("SLA", "Height", "Seed"),
#'                                           calcSimpsonBeta = TRUE)
#' scenarioSelectedMultisite
#' head(scenarioSelectedMultisite$selection$multisite$results)
#' @export
optimizeSelection <- function(x, group = NULL, includeReference = TRUE, maxComb = 1000, calcSimpsonBeta = TRUE, traits = NULL, rao = NULL){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  # Check object class
  # if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
  #   stop("The x argument must be of class simRest or simRestSelect")
  # }
  if(inherits(x, "simRest")){
    xComp <- x$simulation$composition
    xGroup <- x$simulation$group
    xMulti <- x$simulation$multifunctionality
  } else{
    xComp <- x$selection$composition
    xGroup <- x$selection$group
    xMulti <- x$selection$multifunctionality
  }
  # Set the number and the names of result columns
  nColRes <- 0
  colnamesRes <- c()
  if(!is.null(xMulti)){
    # Rownames of multifunctionality
    namesMulti <- xMulti[, 1]
    # Remove names
    xMulti <- xMulti[, -1, drop =  FALSE]
    # Set the number of result columns
    nColRes <- nColRes + 3
    colnamesRes <- c(colnamesRes, "landscapeMultifunctionality", "raoMultifunctionality", "averageFunctions")
  }
  if(calcSimpsonBeta){
    # Set the number of result columns
    nColRes <- nColRes + 4
    colnamesRes <- c(colnamesRes,  "totalDiversity", "alphaDiversity", "betaDiversity", "Fst")
  }
  # Set groups
  if(!is.null(group)){
    groupNames <- xGroup[, group]
  } else{
    groupNames <- rep("sim", nrow(xComp))
  }
  # Merge compositions - simulations and reference
  if(!is.null(x$reference) && includeReference){
    referenceComp  <- x$reference$composition
    # nRef <- nrow(reference)
    template0 <- makeMatrixTemplate(xComp, referenceComp )
    xComp <- reorganizeMatrix(template = template0, xComp, fillNA = TRUE)
    referenceComp  <- reorganizeMatrix(template = template0, referenceComp , fillNA = TRUE)
    # This sequence is important for split the results
    xComp <- rbind(referenceComp , xComp)
    # Multifunctionality
    if(!is.null(xMulti)){
      namesMultiRef <- x$reference$multifunctionality[, 1]
      xMultiRef <- x$reference$multifunctionality[, -1, drop =  FALSE]
      template0 <- makeMatrixTemplate(xMulti, xMultiRef)
      xMulti <- reorganizeMatrix(template = template0, xMulti, fillNA = FALSE)
      xMultiRef <- reorganizeMatrix(template = template0, xMultiRef, fillNA = FALSE)
      # This sequence is important
      xMulti <- rbind.data.frame(xMultiRef, xMulti)
      rownames(xMulti) <- c(namesMultiRef, namesMulti)
    }
    # x$reference$composition <- reference
    dbCombinations <- data.frame(v1 = c(rownames(xComp)), v2 = c(rownames(referenceComp ), groupNames))
  } else{
    if(!is.null(xMulti)){
      rownames(xMulti) <- namesMulti
    }
    dbCombinations <- data.frame(v1 = c(rownames(xComp)), v2 = groupNames)
  }
  # Make groups combinations
  dbCombinations <- makeCombinations(dbCombinations$v1, dbCombinations$v2, minSubset = 1, maxSubset = 1, maxComb = maxComb)
  # Prepare distance matrix to rao 
  if(!is.null(traits) && !is.null(rao)){
    traitsNames <- colnames(traits)
    if(inherits(rao, "list")){
      DIST <- vector("list", length = length(rao))
      for(i in 1:length(rao)){
        if(inherits(rao[[i]], "character")){
          if(!all(rao[[i]] %in% traitsNames)){
            stop("Each element in the rao list must be either a character vector of specifying one or more columns from the traits data frame or a distance matrix")
          }
          traitSub <- scale(traits[, rao[[i]], drop = FALSE] )
          dis <- stats::dist(traitSub)
          DIST[[i]] <-  as.matrix(dis)
        } else if(inherits(rao[[i]], "dist")){
          DIST[[i]] <-  as.matrix(rao[[i]])
        }
      }
      if(is.null(names(rao))){
        names(DIST) <- paste0("rao_", seq_len(length(rao)))
      } else{
        names(DIST) <- paste0("rao_", names(rao))
      }
      nColRes <- nColRes + length(DIST)*4
      colnamesRes <- c(colnamesRes, outer(c("totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional"), paste0("_",names(DIST)), paste0))
    } else{ # If a vector
      if(inherits(rao, "character")){
        if(!all(rao %in% traitsNames)){
          stop("The rao argument must be either a character vector specifying a single columm from the traits data frame, a distance matrix, or a list ontaining these elements")
        }
        traitsSub <- scale(traits[, rao, drop = FALSE] )
        dis <- stats::dist(traitsSub)
        DIST <- as.matrix(dis)
      } else if(inherits(rao, "dist")){
        DIST <- as.matrix(rao)
      }
      nColRes <- nColRes + 4
      colnamesRes <- c(colnamesRes, "totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional")
    }
  }
  resCombinations <- matrix(NA, nrow(dbCombinations), ncol = nColRes)
  for(i in 1:nrow(dbCombinations)){
    resultsTemp <- c()
    # Select which combination
    whichComb <- names(which(dbCombinations[i,]==1))
    # Select species composition
    subComp <- xComp[whichComb,, drop = FALSE]
    if(!is.null(xMulti)){
      subMF <- xMulti[whichComb,, drop = FALSE]
      resultsTemp <- c(resultsTemp, unlist(calcMF(subMF)))
    }
    if(calcSimpsonBeta){
      resultsTemp <- c(resultsTemp, unlist(calcRAO(xComp[whichComb,, drop = FALSE], averages = TRUE)))
    }
    if(!is.null(traits) && !is.null(rao)){
      if(inherits(DIST, "list")){
        for(j in 1:length(DIST)){
          resultsTemp <- c(resultsTemp, unlist(calcRAO(xComp[whichComb,, drop = FALSE], dis = DIST[[j]], averages = TRUE)))
        }
      } else{
        resultsTemp <- c(resultsTemp, unlist(calcRAO(xComp[whichComb,, drop = FALSE], dis = DIST, averages = TRUE)))
      }
    }
    resCombinations[i,] <- resultsTemp
    # require(betapart)
    # scenario$simulation$composition
    # decostand(scenario$simulation$composition, method = "pa")
    # beta.multi(decostand(scenario$simulation$composition, method = "pa")[1:4,], index.family="sor")
    # compTemp <- xComp[whichComb,]
    # compTemp[compTemp>0] <- 1
    # resCombinations[i,] <- unlist(calcRAO2(compTemp))
    # resCombinations[i,] <- unlist(beta.multi(compTemp, index.family="sor"))
    # beta.multi(compTemp, index.family="sor")
    # functional.beta.multi(compTemp, traits = cerrado.mini$traits[,1:4], index.family = "sor")
  }
  # Set names
  rownames(resCombinations) <- rownames(dbCombinations)
  colnames(resCombinations) <- colnamesRes
  # Set results
  x$selection$multisite$combinations <- dbCombinations
  x$selection$multisite$results <- as.data.frame(resCombinations)
  return(x)
}
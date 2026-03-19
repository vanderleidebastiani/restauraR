#' @title Optimise community selection based on a multi-site approach
#' @description Calculate the functional parameters of selected simulated community sets to optimise multi-site simulation selections. This function generates all unique combinations of one simulated community per restoration site and calculates multi-site indices for each combination.
#' @encoding UTF-8
#' @importFrom BAT beta.multi
#' @param x A object of class "simRestSelect" to perform the optimisation.
#' @param siteGroup Character vector specifying a parameter name to define simulation site groups.
#' @param includeReference Logical argument specifying whether to include reference sites in index calculations (default includeReference = TRUE).
#' @param maxComb Maximum number of simulation combinations to generate (default maxComb = 1000). If the total unique combinations exceed this limit, a random sample of combinations is generated.
#' @param calcTaxonomicBeta  Logical argument to specify if calculates taxonomic beta diversity (default calcTaxonomicBeta = TRUE).
#' @param method Method to calculate the beta diversity, partial match to "betaRao", "betaJaccard" or "betaSoerensen" (default method = "betaRao") .
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param beta Character vector specifying trait names to calculate beta diversity, or distance matrix (class dist). This argument can be a list to calculate multiple beta indices using different trait sets or species distance matrices.
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
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}},
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
#'                                 restComp = cerrado$restoration,
#'                                 maxDiver = c("SLA", "Height", "Seed"),
#'                                 constCWM = "BT",
#'                                 rich = c(10, 15),
#'                                 it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado$traits,
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               reference = cerrado$reference)
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
#'                                       siteGroup = "Site",
#'                                       singleSelection = FALSE)
#' scenarioSelected
#' # Optimise selection
#' scenarioSelectedMultisite <- optimiseSelection(scenarioSelected,
#'                                                siteGroup = "Site",
#'                                                traits = cerrado$traits,
#'                                                beta = c("SLA", "Height", "Seed"),
#'                                                calcTaxonomicBeta = TRUE)
#' scenarioSelectedMultisite
#' @export
optimiseSelection <- function(x, siteGroup = NULL, includeReference = TRUE, maxComb = 1000, calcTaxonomicBeta = TRUE, method = "betaRao", traits = NULL, beta = NULL){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  # Check object class
  # if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
  #   stop("The x argument must be of class simRest or simRestSelect")
  # }
  METHOD <- c("betaRao", "betaJaccard", "betaSoerensen")
  methodTest <- pmatch(method, METHOD)
  if (length(methodTest) > 1) {
    stop("Only one method can be specified")
  }
  if (is.na(methodTest)) {
    stop("Invalid method. Choose either betaRao, betaJaccard or betaSoerensen")
  }
  if(methodTest == 2){
    betaMultiMethod <- "jaccard"
  } else if(methodTest == 3) {
    betaMultiMethod <- "soerensen"
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
    xComp <- x$simulation$composition
    # xGroup <- x$simulation$group
    xMulti <- x$simulation$multifunctionality
  } else{
    xPar <- x$selection$results
    xComp <- x$selection$composition
    # xGroup <- x$selection$group
    xMulti <- x$selection$multifunctionality
  }
  # Set the number and the names of result columns
  nColRes <- 1
  colnamesRes <- "totalRichness"
  if(!is.null(xMulti)){
    # Rownames of multifunctionality
    namesMulti <- xMulti[, 1]
    # Remove names
    xMulti <- xMulti[, -1, drop =  FALSE]
    # Set the number of result columns
    nColRes <- nColRes + 3
    colnamesRes <- c(colnamesRes, "landscapeMultifunctionality", "raoMultifunctionality", "averageFunctions")
  }
  if(calcTaxonomicBeta){
    if(methodTest == 1){ # picanteRao
      # Set the number of result columns
      nColRes <- nColRes + 4
      colnamesRes <- c(colnamesRes,  "totalDiversity", "alphaDiversity", "betaDiversity", "Fst")  
    } else{
        # Set the number of result columns
        nColRes <- nColRes + 5
        colnamesRes <- c(colnamesRes,  "Btotal", "Brepl", "Brich", "Bgain", "Bloss")  
    }
  }
  # Set siteGroups
  if(!is.null(siteGroup)){
    # groupNames <- xGroup[, siteGroup]
    groupNames <- xPar[, siteGroup]
  } else{
    groupNames <- rep("sim", nrow(xComp))
  }
  # Merge compositions - simulations and reference
  if(!is.null(x$reference) && includeReference){
    referenceComp  <- x$reference$composition
    # nRef <- nrow(reference)
    template0 <- makeMatrixTemplate(xComp, referenceComp)
    xComp <- rearrangementMatrix(template = template0, xComp, fillNA = TRUE)
    referenceComp  <- rearrangementMatrix(template = template0, referenceComp , fillNA = TRUE)
    # This sequence is important for split the results
    xComp <- rbind(referenceComp , xComp)
    # Multifunctionality
    if(!is.null(xMulti)){
      namesMultiRef <- x$reference$multifunctionality[, 1]
      xMultiRef <- x$reference$multifunctionality[, -1, drop =  FALSE]
      template0 <- makeMatrixTemplate(xMulti, xMultiRef)
      xMulti <- rearrangementMatrix(template = template0, xMulti, fillNA = FALSE)
      xMultiRef <- rearrangementMatrix(template = template0, xMultiRef, fillNA = FALSE)
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
  # Prepare distance matrix to beta 
  if(!is.null(traits) && !is.null(beta)){
    traitsNames <- colnames(traits)
    if(inherits(beta, "list")){
      DIST <- vector("list", length = length(beta))
      for(i in 1:length(beta)){
        if(inherits(beta[[i]], "character")){
          if(!all(beta[[i]] %in% traitsNames)){
            stop("Each element in the beta list must be either a character vector of specifying one or more columns from the traits data frame or a distance matrix")
          }
          traitSub <- scale(traits[, beta[[i]], drop = FALSE] )
          dis <- stats::dist(traitSub)
          DIST[[i]] <-  as.matrix(dis)
        } else if(inherits(beta[[i]], "dist")){
          DIST[[i]] <-  as.matrix(beta[[i]])
        } 
      }
      if(is.null(names(beta))){
        names(DIST) <- paste0("beta_", seq_len(length(beta)))
      } else{
        names(DIST) <- paste0("beta_", names(beta))
      }
      # nColRes <- nColRes + length(DIST)*4
      # colnamesRes <- c(colnamesRes, outer(c("totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional"), paste0("_", names(DIST)), paste0))
      if(methodTest == 1){ # picanteRao
        # Set the number of result columns
        nColRes <- nColRes + length(DIST)*4
        colnamesRes <- c(colnamesRes, outer(c("totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional"), paste0("_", names(DIST)), paste0))
      } else{
        # Set the number of result columns
        nColRes <- nColRes + length(DIST)*5
        colnamesRes <- c(colnamesRes, outer(c("BtotalFunctional", "BreplFunctional", "BrichFunctional", "BgainFunctional", "BlossFunctional"), paste0("_", names(DIST)), paste0))
      }
    } else{ # If a vector
      if(inherits(beta, "character")){
        if(!all(beta %in% traitsNames)){
          stop("The beta argument must be either a character vector specifying a single columm from the traits data frame, a distance matrix, or a list ontaining these elements")
        }
        traitsSub <- scale(traits[, beta, drop = FALSE] )
        dis <- stats::dist(traitsSub)
        DIST <- as.matrix(dis)
      } else if(inherits(beta, "dist")){
        DIST <- as.matrix(beta)
      }
      # nColRes <- nColRes + 4
      # colnamesRes <- c(colnamesRes, "totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional")
      if(methodTest == 1){ # picanteRao
        # Set the number of result columns
        nColRes <- nColRes + 4
        colnamesRes <- c(colnamesRes, "totalFunctionalDiversity", "alphaFunctionalDiversity", "betaFunctionalDiversity", "FstFunctional")
      } else{
        # Set the number of result columns
        nColRes <- nColRes + 5
        colnamesRes <- c(colnamesRes, "BtotalFunctional", "BreplFunctional", "BrichFunctional", "BgainFunctional", "BlossFunctional")
      }
    }
  }
  resCombinations <- matrix(NA, nrow(dbCombinations), ncol = nColRes)
  for(i in 1:nrow(dbCombinations)){
    resultsTemp <- c()
    # Select which combination
    whichComb <- names(which(dbCombinations[i,]==1))
    # Select species composition
    subComp <- xComp[whichComb,, drop = FALSE]
    # Total richness
    resultsTemp <- c(resultsTemp, sum(ifelse(colSums(ifelse(subComp>0, 1, 0), na.rm = TRUE)>0, 1, 0), na.rm = TRUE))
    if(!is.null(xMulti)){
      subMF <- xMulti[whichComb,, drop = FALSE]
      resultsTemp <- c(resultsTemp, unlist(calcMF(subMF)))
    }
    if(calcTaxonomicBeta){
      if(methodTest == 1){ # picanteRao
        resultsTemp <- c(resultsTemp, unlist(calcRAO(subComp, averages = TRUE)))  
      } else{
        if(nrow(subComp)>1){
          resultsTemp <- c(resultsTemp, unlist(BAT::beta.multi(subComp, func = betaMultiMethod, abund = TRUE)[,1]))  
        } else {
          resultsTemp <- c(resultsTemp, rep(NA, 5))
        }
        
      }
    }
    if(!is.null(traits) && !is.null(beta)){
      if(inherits(DIST, "list")){
        for(j in 1:length(DIST)){
          if(methodTest == 1){ # picanteRao
            resultsTemp <- c(resultsTemp, unlist(calcRAO(subComp, sppDist = DIST[[j]], averages = TRUE)))   
          } else{
            if(nrow(subComp)>1){
              resultsTemp <- c(resultsTemp, unlist(BAT::beta.multi(subComp, DIST[[j]], func = betaMultiMethod, abund = TRUE)[,1]))    
            } else {
              resultsTemp <- c(resultsTemp, rep(NA, 5))
            }
            
          }
          
        }
      } else{
        if(methodTest == 1){ # picanteRao
          resultsTemp <- c(resultsTemp, unlist(calcRAO(subComp, sppDist = DIST, averages = TRUE)))  
        } else{
          if(nrow(subComp)>1){
            resultsTemp <- c(resultsTemp, unlist(BAT::beta.multi(subComp, DIST, func = betaMultiMethod, abund = TRUE)[,1]))    
          } else {
            resultsTemp <- c(resultsTemp, rep(NA, 5))
          }
        }
        
      }
    }
    resCombinations[i,] <- resultsTemp
    # unlist(betapart::functional.beta.multi(decostand(subComp, method = "pa"), traits[, beta, drop = FALSE], index.family = "sor"))
    # unlist(betapart::functional.beta.multi(decostand(subComp, method = "pa"), traits[, beta, drop = FALSE], index.family = "jac"))
  }
  # Set names
  rownames(resCombinations) <- rownames(dbCombinations)
  colnames(resCombinations) <- colnamesRes
  # Set results
  x$selection$multisite$combinations <- dbCombinations
  x$selection$multisite$results <- as.data.frame(resCombinations)
  return(x)
}
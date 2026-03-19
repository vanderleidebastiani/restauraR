#' @title Compute and standardise functional parameters in communities
#' @description \code{computeParameters} Calculate basic parameters in each simulated community: richness, count species unavailable, Community Weighted Mean, Community Weighted Variance, Rao's quadratic entropy and functional dissimilarity.
#' 
#' \code{standardiseParameters} Standardises the calculated parameters. Two methods are available: "max", which divides the values by the maximum, and "standardise", which scales the calculated parameters to zero mean and unit variance.
#' 
#' \code{computeMultifunctionality} Computes binary multifunctionality matrix and alpha multifunctionality index based on specific test criteria. 
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @importFrom SYNCSA matrix.t
#' @importFrom stats dist
#' @aliases computeMultifunctionality standardiseParameters
#' @param x A object of class "simRest" or "simRestSelect" to perform communities parameters.
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava Character vector specifying the trait name that indicates the availability of species in traits data (binary: 1 = available, 0 = unavailable).
#' @param cwm Character vector specifying traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param cwv Character vector specifying traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.
#' @param rao Character vector specifying trait names to calculate Rao's Quadratic Entropy, or distance matrix (class dist). This argument can be a list to calculate multiple Rao indices using different trait sets or species distance matrices.
#' @param cost Character vector specifying traits names containing cost per individual for restoration cost estimation.
#' @param dens Character vector specifying traits names containing species planting density information for cost calculations. Used only in the method "proportions".
#' @param traitsFUN A character vector of trait names to be used in the custom analysis (FUN argument).
#' @param FUN A function object to perform the custom analysis.
#' @param ... Other arguments passed to the custom analysis (FUN argument).
#' @param dissimilarity Character vector specifying traits names to calculate dissimilarity with reference sites, or distance matrix (class dist).
#' @param reference Matrix with species composition in the reference sites. NAs not accepted. (default reference = NULL)
#' @param supplementary Matrix with species composition in the supplementary sites. NAs not accepted. (default supplementary = NULL).
#' @param tests Character vector of logical tests for multifunctionality assessment. 
#' @param parameters Character vector of parameters names to standardised.
#' @param method Standardisation method, "max" or "standardise". 
#' @returns A list (class "simRest" or "simRestSelect") with the elements:
#' \item{call}{The arguments used.}
#' \item{simulation$composition}{A matrix with species composition for simulated communities.}
#' \item{simulation$group}{A data frame with complementary information for restoration sites.}
#' \item{simulation$baseline}{A matrix with baseline species composition for simulated communities (contains all zeros when restComp is not provided.)}
#' \item{simulation$results}{A data frame with calculated parameters in each simulated community.}
#' \item{simulation$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{reference$multifunctionality}{A data frame with binary multifunctionality tests to reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' \item{supplementary$multifunctionality}{A data frame with binary multifunctionality tests to supplementary sites.}
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{checkReference}}, \code{\link{simulateCommunities}}, \code{\link{selectCommunities}},
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
#'                          ava = "Available",
#'                          maxDiver = c("SLA", "Height", "Seed"),
#'                          constCWM = "BT",
#'                          rich = c(10, 15),
#'                          it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado$traits,
#'                               ava = "Available",
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               cost = "Cost",
#'                               dens = "Density",
#'                               dissimilarity = c("SLA", "Height", "Seed"),
#'                               reference = cerrado$reference)
#' scenario
#' # Standardise parameters
#' scenario <- standardiseParameters(x = scenario,
#'                                   parameters = "dissimilarity",
#'                                   method = "max")
#' scenario
#' # Compute multifunctionality
#' scenario <- computeMultifunctionality(x = scenario,
#'                                  tests = c("CWM_BT > 5.9",
#'                                            "rao > 0.2"))
#' scenario
#' @export
computeParameters <- function(x, traits, ava = NULL, cwm = NULL, cwv = NULL, rao = NULL, cost = NULL, dens = NULL, traitsFUN = NULL, FUN = NULL, dissimilarity = NULL, reference = NULL, supplementary = NULL, ...){
  # Check object class
  if(!inherits(x, "simRest")){
    stop("The x argument must be of class simRest")
  }
  # Check data
  checkRestauraRData(traits = traits, 
                     restComp = NULL, 
                     restGroup = NULL,
                     reference = reference, 
                     supplementary = supplementary,
                     sppDist = rao,
                     asList = FALSE)
  # Check dissimilarity if provided
  checkRestauraRData(traits = traits, 
                     restComp = NULL, 
                     restGroup = NULL,
                     reference = NULL, 
                     supplementary = NULL,
                     sppDist = dissimilarity,
                     asList = FALSE)
  composition <- x$simulation$composition
  nSim <- nrow(composition)
  traitsNames <- colnames(traits)
  # Check if all number are integer
  anyPositive <- length(composition[composition>0])>0
  allInteger <- all(composition%%1 == 0)
  rowCheck <- isTRUE(all.equal(rowSums(composition), rep(1, nrow(composition)), check.attributes = FALSE, check.class = FALSE))
  if(anyPositive && !allInteger && !rowCheck){
    stop("Simulation using species proportions must sum to 1 for each site")
  }
  
  
  
  
  # # Extract baseline
  # baseline <- x$simulation$baseline
  # # Calculate additions
  # # If proportions
  # if(!allInteger){
  #   compAdditions <- (composition*2) - baseline
  # } else{ # If counts
  #   compAdditions <- composition - baseline
  # }
  # Merge compositions - simulations, reference and supplementary
  if(!is.null(reference) && is.null(supplementary)){
    nRef <- nrow(reference)
    reference/sum(reference)
    anyPositiveReference <- length(reference[reference>0])>0
    allIntegerReference <- all(reference%%1 == 0)
    rowCheckReference <- isTRUE(all.equal(rowSums(reference), rep(1, nrow(reference)), check.attributes = FALSE, check.class = FALSE))
    if(anyPositiveReference && (allIntegerReference != allInteger)){
      stop("Reference matrix must contain only species proportions or raw abundances, consistent with the simulation method parameter")
    }
    if(anyPositiveReference && !allIntegerReference && !rowCheckReference){
      stop("Reference matrix using species proportions must sum to 1 for each site")
    }
    template0 <- makeMatrixTemplate(composition, reference)
    composition <- rearrangementMatrix(template = template0, composition, fillNA = TRUE)
    reference <- rearrangementMatrix(template = template0, reference, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition)
    x$reference$composition <- reference
  }
  if(!is.null(supplementary) && is.null(reference)){
    nSupple <- nrow(supplementary)
    anyPositiveSupplementary <- length(supplementary[supplementary>0])>0
    allIntegerSupplementary <- all(supplementary%%1 == 0)
    rowCheckSupplementary <- isTRUE(all.equal(rowSums(supplementary), rep(1, nrow(supplementary)), check.attributes = FALSE, check.class = FALSE))
    if(anyPositiveSupplementary && (allIntegerSupplementary != allInteger)){
      stop("Supplementary matrix must contain only species proportions or raw abundances, consistent with the simulation method parameter")
    }
    if(anyPositiveSupplementary && !allIntegerSupplementary && !rowCheckSupplementary){
      stop("Supplementary matrix using species proportions must sum to 1 for each site")
    }
    template0 <- makeMatrixTemplate(composition, supplementary)
    composition <- rearrangementMatrix(template = template0, composition, fillNA = TRUE)
    supplementary <- rearrangementMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(composition, supplementary)
    x$supplementary$composition <- supplementary
  }
  if(!is.null(reference) && !is.null(supplementary)){
    nRef <- nrow(reference)
    nSupple <- nrow(supplementary)
    anyPositiveReference <- length(reference[reference>0])>0
    allIntegerReference <- all(reference%%1 == 0)
    rowCheckReference <- isTRUE(all.equal(rowSums(reference), rep(1, nrow(reference)), check.attributes = FALSE, check.class = FALSE))
    if(anyPositiveReference && (allIntegerReference != allInteger)){
      stop("Reference matrix must contain only species proportions or raw abundances, consistent with the simulation method parameter")
    }
    if(anyPositiveReference && !allIntegerReference && !rowCheckReference){
      stop("Reference matrix using species proportions must sum to 1 for each site")
    }
    anyPositiveSupplementary <- length(supplementary[supplementary>0])>0
    allIntegerSupplementary <- all(supplementary%%1 == 0)
    rowCheckSupplementary <- isTRUE(all.equal(rowSums(supplementary), rep(1, nrow(supplementary)), check.attributes = FALSE, check.class = FALSE))
    if(anyPositiveSupplementary && (allIntegerSupplementary != allInteger)){
      stop("Supplementary matrix must contain only species proportions or raw abundances, consistent with the simulation method parameter")
    }
    if(anyPositiveSupplementary && !allIntegerSupplementary && !rowCheckSupplementary){
      stop("Supplementary matrix using species proportions must sum to 1 for each site")
    }
    template0 <- makeMatrixTemplate(composition, reference, supplementary)
    composition <- rearrangementMatrix(template = template0, composition, fillNA = TRUE)
    reference <- rearrangementMatrix(template = template0, reference, fillNA = TRUE)
    supplementary <- rearrangementMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition, supplementary)
    x$reference$composition <- reference
    x$supplementary$composition <- supplementary
  }
  # # Calculate species proportions
  # composition2 <- composition #to calculate CWM2
  # composition <- sweep(composition, MARGIN = 1, rowSums(composition), FUN = "/")
  # Organize traits
  matchNames <- match(colnames(composition), rownames(traits))
  traits <- as.data.frame(traits[matchNames, , drop = FALSE])
  # Calculate parameters
  out <- NULL
  # Count species unavailable
  if(!is.null(ava)){
    if(!inherits(ava, "character") || !all(ava %in% traitsNames) || length(ava)>1){
      stop("The ava argument must be a character vector specifying a single columm from the traits data frame")
    }
    UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(traits[,ava])] > 0) )
    out <- cbind(out, unavailable = UNA)
  }
  # Richness
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  # Simpson diversity
  diverSimpson <- SYNCSA::rao.diversity(comm = composition)$Simpson
  out <- cbind(out, simpson = diverSimpson)
  # CWM
  if(!is.null(cwm)){
    if(!inherits(cwm, "character") || !all(cwm %in% traitsNames)){
      stop("The cwm argument must be a character vector specifying one or more columns from the traits data frame")
    }
    traitsSub <- traits[, cwm, drop = FALSE]
    CWM <- SYNCSA::matrix.t(composition, traitsSub, scale = FALSE)$matrix.T
    colnames(CWM) <- paste0("CWM_", colnames(CWM))
    out <- cbind(out, CWM)
  }
  # if(!is.null(cwm2)){
  #   if(!inherits(cwm2, "character") || !all(cwm2 %in% traitsNames)){
  #     stop("The cwm2 argument must be a character vector specifying one or more columns from the trait data frame")
  #   }
  #   traitSub <- traits[, cwm2, drop = FALSE]
  #   CWM2 <- calcCWM2(composition, traitSub)
  #   colnames(CWM2) <- paste0("CWM2_", colnames(CWM2))
  #   out <- cbind(out, CWM2)
  # }
  # CWV
  if(!is.null(cwv)){
    if(!inherits(cwv, "character") || !all(cwv %in% traitsNames)){
      stop("The cwv argument must be a character vector specifying one or more columns from the traits data frame")
    }
    traitsSub <- traits[, cwv, drop = FALSE]
    CWV <- calcCWV(composition, traitsSub)
    colnames(CWV) <- paste0("CWV_", colnames(CWV))
    out <- cbind(out, CWV)
  }
  # Rao diversity
  if(!is.null(rao)){
    # If a list
    if(inherits(rao, "list")){
      RAOlist <- NULL
      for(i in 1:length(rao)){
        if(inherits(rao[[i]], "character")){
          if(!all(rao[[i]] %in% traitsNames)){
            stop("Each element in the rao list must be either a character vector of specifying one or more columns from the traits data frame or a distance matrix")
          }
          traitSub <- scale(traits[, rao[[i]], drop = FALSE] )
          dis <- stats::dist(traitSub)
          RAOtemp <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(dis))$PhyRao
        } else if(inherits(rao[[i]], "dist")){
          RAOtemp <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(rao[[i]]))$PhyRao
        }
        RAOlist <- cbind(RAOlist, RAOtemp)
      }
      if(is.null(names(rao))){
        colnames(RAOlist) <- paste0("rao_", seq_len(length(rao)))
      } else{
        colnames(RAOlist) <- paste0("rao_", names(rao))
      }
      out <- cbind(out, RAOlist)
    } else{ # If a vector
      if(inherits(rao, "character")){
        if(!all(rao %in% traitsNames)){
          stop("The rao argument must be either a character vector specifying a single columm from the traits data frame, a distance matrix, or a list ontaining these elements")
        }
        traitsSub <- scale(traits[, rao, drop = FALSE] )
        dis <- stats::dist(traitsSub)
        RAO <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(dis))$PhyRao
      } else if(inherits(rao, "dist")){
        RAO <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(rao))$PhyRao
      }
      out <- cbind(out, rao = RAO)
    }
  }
  # Cost - It require species cost and planting density 
  # if(!is.null(cost) || !is.null(dens)){
  if(!is.null(cost)){
    if(!inherits(cost, "character") || !all(cost %in% traitsNames) || length(cost)>1){
      stop("The cost argument must be a character vector specifying a single columm from the traits data frame")
    }
    # costVect <- traits[, cost]
    costMat <- as.matrix(traits[, cost, drop = FALSE])
    # If not all integer (proportions method)
    # allInteger <- FALSE
    if(!allInteger){
      if(!is.null(dens)){
        if(!inherits(dens, "character") || !all(dens %in% traitsNames) || length(dens)>1){
          stop("The dens argument must be a character vector specifying a single columm from the traits data frame")
        }
        # densVect <- traits[, dens]
        densMat <- as.matrix(traits[, dens, drop = FALSE])
        costMat <- costMat*densMat
        # COST <- apply(composition, 1, FUN = function(p){
        #   COST_i <- sum(p*costVect*densVect, na.rm = TRUE)
        #   return(COST_i)
        # })
        COST <- composition%*%costMat
        out <- cbind(out, cost = COST)
      } #else{
      # stop("The dens argument must be a character vector specifying a single columm from the traits data frame")
      # }
    } else{ # # If all integer (individuals method)
      # COST <- apply(composition, 1, FUN = function(p){
      #   COST_i <- sum(p*costVect, na.rm = TRUE)
      #   return(COST_i)
      # })
      COST <- composition%*%costMat
      out <- cbind(out, cost = COST)
    }
  }
  #  Dissimilatity
  if(!is.null(reference)){
    nRef <- nrow(reference)
    resDis <- calcRAO(composition, nRef = nRef)
    # Remove matrix diagonal
    # diag(resDis) <- NA
    diagIndex <- cbind(seq(nRef), seq(nRef))
    resDis[diagIndex] <- NA
    # # Keep only values related to reference sites
    # resDis <- resDis[, seq.int(nRef), drop = FALSE]
    # Calculate mean dissimilarities
    resDis <- apply(resDis, MARGIN = 1, mean, na.rm = TRUE)
    out <- cbind(out, dissimilarity = resDis)
  }
  # Functional dissimilatity
  if(!is.null(dissimilarity) && !is.null(reference)){
    nRef <- nrow(reference)
    if(inherits(dissimilarity, "character")){
      if(!all(dissimilarity %in% traitsNames)){
        stop("The dissimilarity argument must be either a character vector specifying a single columm from the traits data frame or a distance matrix")
      }
      traitsSub <- scale(traits[, dissimilarity, drop = FALSE])
      dis <- stats::dist(traitsSub)
      # resDis <- as.matrix(adiv::discomQE(composition, dis = dis, formula = "QE"))
      resDis <- calcRAO(composition, sppDist = dis, nRef = nRef)
    } else if(inherits(dissimilarity, "dist")){
      # Calculate dissimilarities between communities
      # resDis <- as.matrix(adiv::discomQE(composition, dis = dissimilarity, formula = "QE"))
      resDis <- calcRAO(composition, sppDist = dissimilarity, nRef = nRef)
    }
    # Remove matrix diagonal
    diagIndex <- cbind(seq(nRef), seq(nRef))
    resDis[diagIndex] <- NA
    # diag(resDis) <- NA
    # Keep only values related to reference sites
    # resDis <- resDis[, seq.int(nRef), drop = FALSE]
    # Calculate mean dissimilarities
    resDis <- apply(resDis, MARGIN = 1, mean, na.rm = TRUE)
    out <- cbind(out, functionalDissimilarity = resDis)
  }
  # Custom function
  if(!is.null(traitsFUN) && !is.null(FUN)){
    if(!inherits(traitsFUN, "character") || !all(traitsFUN %in% traitsNames)){
      stop("The traitsFUN argument must be a character vector specifying one or more columns from the traits data frame")
    }
    traitsSub <- traits[, traitsFUN, drop = FALSE]
    resFUN <- lapply(1, FUN = FUN, comm = composition, traits = traitsSub, ...)[[1]]
    out <- cbind(out, resFUN)
  }
  # Results organization
  if(!is.null(reference) || !is.null(supplementary)){
    if(!is.null(reference) && is.null(supplementary)){
      x$reference$results <- cbind.data.frame(out[seq.int(nRef), , drop = FALSE])
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[-1*seq.int(nRef), , drop = FALSE])  
    }
    if(!is.null(supplementary) && is.null(reference)){
      nTotal <- nrow(out)
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[seq.int(nTotal-nSupple), , drop = FALSE])  
      x$supplementary$results <- cbind.data.frame(out[-1*seq.int(nTotal-nSupple), , drop = FALSE])
    }
    if(!is.null(reference) && !is.null(supplementary)){
      nTotal <- nrow(out)
      x$reference$results <- cbind.data.frame(out[seq.int(nRef), , drop = FALSE])
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[seq.int(nTotal)[(nRef+1):(nSim+nRef)], , drop = FALSE])
      x$supplementary$results <- cbind.data.frame(out[seq.int(nTotal)[(nRef+nSim+1):nTotal], , drop = FALSE])
    }
  } else {
    x$simulation$results <- cbind.data.frame(x$simulation$group, out)  
  }
  rownames(x$simulation$results) <- NULL
  return(x)
}
#' @title Compute functional parameters in communities
#' @description Calculate basic parameters in each community: richness, count species unavailable, Community Weighted Mean, Community Weighted Variance, Rao Quadratic Entropy, functional dissimilarity and multifunctionality.
#' @details
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @importFrom fundiversity fd_raoq
#' @importFrom SYNCSA matrix.t
#' @importFrom adiv discomQE
#' @aliases computeDissimilarity computeMultifunctionality
#' @param x A object of class "simRest" or "simRestSelect" to perform calculate communities parameters.
#' @param trait data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param cwm A vector with traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param cwv A vector with traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist).
#' @param cost A vector with trait name with of species cost per individual
#' @param dens A vector with trait name with species planting density
#' @param stan A vector with parameters names to specify which parameters should be standardized by the maximum.
#' @param reference A matrix with species proportions in the reference sites. NAs not accepted. (default reference = NULL)
#' @param supplementary A matrix with species proportions in the supplementary sites. NAs not accepted. (default supplementary = NULL).
#' @param tests A vector with multifunctionality criteria to be performed. 
#' @returns A list (class "simRest" or "simRestSelect") with the elements:
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
computeParameters <- function(x, trait, ava, cwm, cwv, rao, cost, dens, stan, reference = NULL, supplementary = NULL){
  # Check object class
  if(!inherits(x, "simRest")){
    stop("x must be of the simRest class")
  }
  # inherits(ava, "character")
  composition <- x$simulation$composition
  nSim <- nrow(composition)
  # Merge compositions - simulations, reference and supplementary
  if(!is.null(reference) && is.null(supplementary)){
    nRef <- nrow(reference)
    template0 <- makeMatrixTemplate(composition, reference)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition)
    x$reference$composition <- reference
  }
  if(!is.null(supplementary) && is.null(reference)){
    nSupple <- nrow(supplementary)
    template0 <- makeMatrixTemplate(composition, supplementary)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    supplementary <- reorganizeMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(composition, supplementary)
    x$supplementary$composition <- supplementary
  }
  if(!is.null(reference) && !is.null(supplementary)){
    nRef <- nrow(reference)
    nSupple <- nrow(supplementary)
    template0 <- makeMatrixTemplate(composition, reference, supplementary)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    supplementary <- reorganizeMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition, supplementary)
    x$reference$composition <- reference
    x$supplementary$composition <- supplementary
  }
  # Organize traits
  trait <- SYNCSA::organize.syncsa(comm = composition, traits = trait, check.comm = FALSE)$traits
  # Calculate parameters
  out <- NULL
  # Count species unavailable
  if(!missing(ava)){
    if(inherits(ava, 'character')){
      UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(trait[,ava])] > 0) )
      out <- cbind(out, unavailable = UNA)
    }
  }
  # Richness
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  # CWM
  if(!missing(cwm)){
    if(inherits(cwm, 'character')){
      traitSub <- trait[, cwm, drop = FALSE]
      CWM <- SYNCSA::matrix.t(composition, traitSub, scale = FALSE)$matrix.T
      colnames(CWM) <- paste0("CWM_", colnames(CWM))
      out <- cbind(out, CWM)
    }
  }
  # CWV
  if(!missing(cwv)){
    if(inherits(cwv, 'character')){
      traitSub <- trait[,cwv, drop=FALSE]
      CWV <- FCWV(composition, traitSub)
      colnames(CWV) <- paste0("CWV_", colnames(CWV))
      out <- cbind(out, CWV)
    }
  }
  # Rao diversity
  if(!missing(rao)){
    if(inherits(rao, 'character')){
      traitSub <- scale(trait[, rao, drop = FALSE] )
      RAO <- fundiversity::fd_raoq(traitSub, composition)$Q
    } else if(inherits(rao, 'dist')){
      RAO <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao)$Q
    }
    out <- cbind(out, rao = RAO)
  }
  # Cost - It require species cost and planting density
  if(!missing(cost) || !missing(dens)){
    costVect <- trait[, cost]
    densVect <- trait[, dens]
    COST <- apply(composition, 1, FUN = function(p){
      COST_i <- sum(p*costVect*densVect, na.rm = TRUE)
      return(COST_i)
    })
    out <- cbind(out, cost = COST)
  }
  # Standardization
  if(!missing(stan)){
    out[ , stan] <- out[ , stan, drop = FALSE]/max(out[ , stan, drop = FALSE])
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
  return(x)
}
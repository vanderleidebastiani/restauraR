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
#' @param cost
#' @param dens
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
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  composition <- x$simulation$composition
  nSim <- nrow(composition)
  if(!is.null(reference) && is.null(supplementary)){
    rowNameComposition <- rownames(composition)
    rowNameRef <- rownames(reference)
    nRef <- nrow(reference)
    template1 <- composition[0,]
    template2 <- reference[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    reference <- data.table::rbindlist(list(template0, as.data.table(reference)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    reference <- as.matrix(reference)
    rownames(composition) <- rowNameComposition
    rownames(reference) <- rowNameRef
    composition[is.na(composition)] <- 0
    reference[is.na(reference)] <- 0
    composition <- rbind(reference, composition)
    x$reference$composition <- reference
  }
  if(!is.null(supplementary) && is.null(reference)){
    rowNameComposition <- rownames(composition)
    rowNameSupple <- rownames(supplementary)
    nSupple <- nrow(supplementary)
    template1 <- composition[0,]
    template2 <- supplementary[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    supplementary <- data.table::rbindlist(list(template0, as.data.table(supplementary)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    supplementary <- as.matrix(supplementary)
    rownames(composition) <- rowNameComposition
    rownames(supplementary) <- rowNameSupple
    composition[is.na(composition)] <- 0
    supplementary[is.na(supplementary)] <- 0
    composition <- rbind(composition, supplementary)
    x$supplementary$composition <- supplementary
  }
  if(!is.null(reference) && !is.null(supplementary)){
    rowNameComposition <- rownames(composition)
    rowNameRef <- rownames(reference)
    rowNameSupple <- rownames(supplementary)
    nRef <- nrow(reference)
    nSupple <- nrow(supplementary)
    template0 <- list(composition[0,], reference[0,], supplementary[0,])
    template0 <- lapply(template0, as.data.table, keep.rownames = TRUE)
    template0 <- data.table::rbindlist(template0, use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    reference <- data.table::rbindlist(list(template0, as.data.table(reference)), use.names = TRUE, fill = TRUE)
    supplementary <- data.table::rbindlist(list(template0, as.data.table(supplementary)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    reference <- as.matrix(reference)
    supplementary <- as.matrix(supplementary)
    rownames(composition) <- rowNameComposition
    rownames(reference) <- rowNameRef
    rownames(supplementary) <- rowNameSupple
    composition[is.na(composition)] <- 0
    reference[is.na(reference)] <- 0
    supplementary[is.na(supplementary)] <- 0
    composition <- rbind(reference, composition, supplementary)
    x$reference$composition <- reference
    x$supplementary$composition <- supplementary
  }
  # Calculate parameters
  out <- NULL
  if(!missing(ava)){
    if(inherits(ava, 'character')){
      UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(trait[,ava])] > 0) )
      out <- cbind(out, unavailable = UNA)
    }
  }
  
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  
  # CWM
  if(!missing(cwm)){
    if(inherits(cwm, 'character')){
      traitSub <- trait[,cwm, drop = FALSE]
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
      traitSub <- scale(trait[,rao, drop=FALSE] )
      RAO <- fundiversity::fd_raoq(traitSub, composition)$Q
    } else if(inherits(rao, 'dist')){
      RAO <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao)$Q
    }
    out <- cbind(out, rao = RAO)
  }
  # Cost
  if(!missing(cost)){
    COST <- apply(x, 1, FUN=function(p){
      COST_i <- sum(p*cost*dens, na.rm = TRUE)
      return(COST_i)
    })
    out <- cbind(out, cost = COST)
  }
  
  if(!missing(stan)){
    out[ , stan] <- out[ , stan, drop = FALSE]/max(out[ , stan, drop = FALSE])
  } 
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
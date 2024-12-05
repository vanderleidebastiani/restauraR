#' @rdname computeParameters
#' @include computeParameters.R
#' @encoding UTF-8
#' @export
computeDissimilarity <- function(x, trait){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    composition <- x$simulation$composition
  } else{
    composition <- x$selection$composition
  }
  # Get reference composition
  reference <- x$reference$composition
  if(is.null(reference)){
    stop("x must include species composition in reference sites")
  }
  # Basic information
  nSim <- nrow(composition)
  nRef <- nrow(reference)
	if(inherits(trait, 'data.frame') || inherits(trait, 'matrix')){
		dis <- stats::dist(scale(trait))
	} else if(inherits(trait, 'dist')){
		dis <- trait
	}
  # Get supplementary composition
  supplementary <- x$supplementary$composition
  if(is.null(supplementary)){
    template0 <- makeMatrixTemplate(composition, reference)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition)
  } else {
    nSupple <- nrow(supplementary)
    template0 <- makeMatrixTemplate(composition, reference, supplementary)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    supplementary <- reorganizeMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition, supplementary)
  }
  # Calculate species proportions
  composition <- sweep(composition, MARGIN = 1, rowSums(composition), FUN = "/")
  # Calculate dissimilarities between communities
  resDis <- as.matrix(adiv::discomQE(composition, dis, formula = "QE"))
  # Remove matrix diagonal
  diag(resDis) <- NA
  # Keep only values related to reference sites
  resDis <- resDis[, seq.int(nRef), drop = FALSE]
  # Calculate mean dissimilarities
  resDis <- apply(resDis, MARGIN = 1, mean, na.rm = TRUE)
  # Standardise
  # resDis <- resDis/max(resDis)
  # Results organization
  if(is.null(supplementary)){
    if(inherits(x, "simRest")){
      x$simulation$results$dissimilarity <- resDis[-1*seq.int(nRef)]
    } else{
      x$selection$results$dissimilarity <- resDis[-1*seq.int(nRef)]
    }
    x$reference$results$dissimilarity <- resDis[seq.int(nRef)]
    
  } else{
    nTotal <- length(resDis)
    if(inherits(x, "simRest")){
      x$simulation$results$dissimilarity <- resDis[seq.int(nTotal)[(nRef+1):(nSim+nRef)]]
    } else{
      x$selection$results$dissimilarity <- resDis[seq.int(nTotal)[(nRef+1):(nSim+nRef)]]
    }
    x$reference$results$dissimilarity <- resDis[seq.int(nRef)]
    x$supplementary$results$dissimilarity <- resDis[seq.int(nTotal)[(nRef+nSim+1):nTotal]]
  }
	return(x)
}
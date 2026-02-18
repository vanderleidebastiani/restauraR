#' @rdname computeParameters
#' @include computeParameters.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
standardiseParameters <- function(x, parameters = NULL, method = c("max", "standardise")){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  # Check method
  stMETHOD <- c("max", "standardise")
  stmethod <- pmatch(method, stMETHOD)
  if (length(stmethod) > 1) {
    stop("Only one method can be specified")
  }
  if (is.na(stmethod)) {
    stop("Invalid method. Choose either max or standardise")
  }
  if(inherits(x, "simRest")){
    simulationSlot <- TRUE # save in simulation slot
    xPar <- x$simulation$results
  } else{
    simulationSlot <- FALSE # save in selection slot
    xPar <- x$selection$results
  }
  varNames <- colnames(xPar)
  if(!is.null(parameters)){
    if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
      stop("The parameters argument must be a character vector specifying one or more column names from the results")
    }
    xPar <- xPar[, parameters, drop = FALSE]
    nSim <- nrow(xPar)
    # Merge results - simulations, reference and supplementary
    if(!is.null(x$reference$results) && is.null(x$supplementary$results)){
      reference <- x$reference$results[, parameters, drop = FALSE]
      nRef <- nrow(reference)
      # This sequence is important for split the results
      xPar <- rbind(reference, xPar)
    }
    if(!is.null(x$supplementary$results) && is.null(x$reference$results)){
      supplementary <- x$supplementary$results[, parameters, drop = FALSE]
      nSupple <- nrow(supplementary)
      # This sequence is important for split the results
      xPar <- rbind(xPar, supplementary)
    }
    if(!is.null(x$reference$results) && !is.null(x$supplementary$results)){
      reference <- x$reference$results[, parameters, drop = FALSE]
      nRef <- nrow(reference)
      supplementary <- x$supplementary$results[, parameters, drop = FALSE]
      nSupple <- nrow(supplementary)
      # This sequence is important for split the results
      xPar <- rbind(reference, xPar, supplementary)
    }
    if(stmethod == 1){
      xPar <- sweep(xPar, MARGIN = 2, STATS = apply(xPar, MARGIN = 2, max, na.rm = TRUE), FUN = "/")
    }
    if(stmethod == 2){
      xPar <- scale(xPar)
    }
    # Results organization
    if(!is.null(x$reference$results) || !is.null(x$supplementary$results)){
      if(!is.null(x$reference$results) && is.null(x$supplementary$results)){
        x$reference$results[, parameters] <- xPar[seq.int(nRef), , drop = FALSE]
        if(simulationSlot){
          x$simulation$results[, parameters] <- xPar[-1*seq.int(nRef), , drop = FALSE]  
        } else{
          x$selection$results[, parameters] <- xPar[-1*seq.int(nRef), , drop = FALSE]
        }
      }
      if(!is.null(x$supplementary$results) && is.null(x$reference$results)){
        nTotal <- nrow(xPar)
        if(simulationSlot){
          x$simulation$results[, parameters] <- xPar[seq.int(nTotal-nSupple), , drop = FALSE]  
        } else{
          x$selection$results[, parameters] <- xPar[seq.int(nTotal-nSupple), , drop = FALSE]
        }
        x$supplementary$results[, parameters] <- xPar[-1*seq.int(nTotal-nSupple), , drop = FALSE]
      }
      if(!is.null(x$reference$results) && !is.null(x$supplementary$results)){
        nTotal <- nrow(xPar)
        x$reference$results[, parameters] <- xPar[seq.int(nRef), , drop = FALSE]
        if(simulationSlot){
          x$simulation$results[, parameters] <- xPar[seq.int(nTotal)[(nRef+1):(nSim+nRef)], , drop = FALSE]  
        } else{
          x$selection$results[, parameters] <- xPar[seq.int(nTotal)[(nRef+1):(nSim+nRef)], , drop = FALSE]
        }
        x$supplementary$results[, parameters] <- xPar[seq.int(nTotal)[(nRef+nSim+1):nTotal], , drop = FALSE]
      }
    } else {
      if(simulationSlot){
        x$simulation$results[, parameters] <- xPar  
      } else{
        x$selection$results[, parameters] <- xPar
      }
    }
  }
  return(x)
}
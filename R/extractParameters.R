#' @title Internal function to extract calculated parameters for plotting functions
#' @encoding UTF-8
#' @param x An object of class "simRest" or "simRestSelect" to extract the results.
#' @param parameters Character vector specifying the variable name for extraction If NULL, all results are extracted (default parameters = NULL).
#' @param showSimulation Logical indicating whether to include simulated sites from extraction (default showSimulation = TRUE).
#' @param showReference Logical indicating whether to include reference sites from extraction (default showReference = TRUE).
#' @param showSupplementary Logical indicating whether to include supplementary sites from extraction (default showSupplementary = TRUE).
#' @returns A data.frame with the parameter extracted, including the columns ResultType, Simulation and Site.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{viewResults}}
#' @keywords InternalFunction
extractParameters <- function(x, parameters = NULL, showSimulation = TRUE, showReference = TRUE, showSupplementary = TRUE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(inherits(x, "simRest")){
    # simulationSlot <- TRUE # save in simulation slot
    xPar <- x$simulation$results
  } else{
    # simulationSlot <- FALSE # save in selection slot
    xPar <- x$selection$results
  }
  varNames <- colnames(xPar)
  if(!"Site" %in% varNames){
    xPar$Site <- NA
  } 
  xPar$ResultType <- "Simulation"
  if(!showSimulation){
    xPar <- xPar[0, , drop = FALSE]
  }
  # nSim <- nrow(xPar)
  if(is.null(parameters)){
    parameters <- varNames[!varNames %in% c("Simulation", "Site")]
  } else{
    if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
      stop("The parameters argument must be a character vector specifying one or more column names from the results")
    }
  }
  xPar <- xPar[, c("ResultType", "Simulation", "Site", parameters), drop = FALSE]
  # Merge results - simulations, reference and supplementary
  if(!is.null(x$reference$results) && is.null(x$supplementary$results) && showReference){
    reference <- x$reference$results
    reference$Site <- rownames(reference)
    reference$ResultType <- "Reference"
    reference$Simulation <- NA
    reference <- reference[, c("ResultType", "Simulation", "Site", parameters), drop = FALSE]
    # nRef <- nrow(reference)
    # This sequence is important for split the results
    xPar <- rbind(reference, xPar)
  }
  if(!is.null(x$supplementary$results) && is.null(x$reference$results) && showSupplementary){
    supplementary <- x$supplementary$results
    supplementary$Site <- rownames(supplementary)
    supplementary$ResultType <- "Supplementary"
    supplementary$Simulation <- NA
    supplementary <- supplementary[, c("ResultType", "Simulation", "Site", parameters), drop = FALSE]
    # nSupple <- nrow(supplementary)
    # This sequence is important for split the results
    xPar <- rbind(xPar, supplementary)
  }
  if(!is.null(x$reference$results) && !is.null(x$supplementary$results)){
    reference <- x$reference$results
    reference$Site <- rownames(reference)
    reference$ResultType <- "Reference"
    reference$Simulation <- NA
    reference <- reference[, c("ResultType", "Simulation", "Site", parameters), drop = FALSE]
    # nRef <- nrow(reference)
    supplementary <- x$supplementary$results
    supplementary$Site <- rownames(supplementary)
    supplementary$ResultType <- "Supplementary"
    supplementary$Simulation <- NA
    supplementary <- supplementary[, c("ResultType", "Simulation", "Site", parameters), drop = FALSE]
    # nSupple <- nrow(supplementary)
    if(showReference && showSupplementary){
      # This sequence is important for split the results
      xPar <- rbind(reference, xPar, supplementary)  
    } else{
      if(showReference && !showSupplementary){
        xPar <- rbind(reference, xPar)
      } else{
        if(showSupplementary && !showReference){
          xPar <- rbind(xPar, supplementary)
        }
      }
    }
  }
  return(xPar)
}
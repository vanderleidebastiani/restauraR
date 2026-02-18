#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
summary.simRest <- function(object, ...) {
  RES <- list()
  # Pool size
  RES$poolSize <- ncol(object$simulation$composition)
  # Number of simulations
  RES$nSim <- nrow(object$simulation$composition)
  # Reference communities
  RES$hasRef <- ifelse(is.null(object$reference), FALSE, TRUE)
  # Supplementary communities
  RES$hasSupple <- ifelse(is.null(object$supplementary), FALSE, TRUE)
  # Summary parameters
  if(!is.null(object$simulation$results)) {
    RES$summaryResults <- resSummary(object$simulation$results, ...)  
  } else{
    RES$summaryResults <- NULL
  }
  # Summary multifunctionality
  if(!is.null(object$simulation$multisite$results)) {
    RES$summaryMultifunctionality <- resSummary(object$simulation$multifunctionality, ...)[2, -1]
  } else{
    RES$summaryMultifunctionality <- NULL
  }
  # Summary multisite parameters
  if(!is.null(object$simulation$multisite$results)) {
    RES$summaryMultisiteResults <- resSummary(object$simulation$multisite$results, ...)
  } else{
    RES$summaryMultisiteResults <- NULL
  }
  class(RES) <- "summarySimRest"
  return(RES)
}

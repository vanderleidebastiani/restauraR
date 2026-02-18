#' @rdname selectCommunities
#' @include selectCommunities.R
#' @encoding UTF-8
#' @export
summary.simRestSelect <- function(object, ...) {
  RES <- list()
  # Pool size
  RES$poolSize <- ncol(object$selection$composition)
  # Number of simulations
  RES$nSim <- nrow(object$selection$composition)
  # Reference communities
  RES$hasRef <- ifelse(is.null(object$reference), FALSE, TRUE)
  # Supplementary communities
  RES$hasSupple <- ifelse(is.null(object$supplementary), FALSE, TRUE)
  # Summary parameters
  if(!is.null(object$selection$results)) {
    RES$summaryResults <- resSummary(object$selection$results, ...)  
  } else{
    RES$summaryResults <- NULL
  }
  # Summary multifunctionality
  if(!is.null(object$selection$multisite$results)) {
    RES$summaryMultifunctionality <- resSummary(object$selection$multifunctionality, ...)[2, -1]
  } else{
    RES$summaryMultifunctionality <- NULL
  }
  # Summary multisite parameters
  if(!is.null(object$selection$multisite$results)) {
    RES$summaryMultisiteResults <- resSummary(object$selection$multisite$results, ...)  
  } else{
    RES$summaryMultisiteResults <- NULL
  }
  class(RES) <- "summarySimRestSelect"
  return(RES)
}
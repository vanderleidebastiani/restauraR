#' @rdname selectCommunities
#' @include selectCommunities.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
print.summarySimRestSelect <- function(x, ...) {
  cat("Overview:\n")
  cat(" Species pool size: ")
  cat(x$poolSize, "\n")
  cat(" Number of simulations selected: ")
  cat(x$nSim, "\n")
  cat(" Reference communities: ")
  cat(ifelse(x$hasRef, "Yes", "No"), "\n")
  cat(" Supplementary communities: ")
  cat(ifelse(x$hasSupple, "Yes", "No"), "\n")
  if(!is.null(x$summaryResults)) {
    cat(" Summary parameters: \n")
    print(x$summaryResults, ...)
    cat("\n")
  } else{
    cat(" Parameters results: ")
    cat("No", "\n\n")
  }
  if(!is.null(x$summaryMultifunctionality)) {
    cat(" Summary multifunctionality: \n")
    print(x$summaryMultifunctionality, ...)
    cat("\n")
  } else{
    cat(" Multifunctionality results: ")
    cat("No", "\n\n")
  }
  if(!is.null(x$summaryMultisiteResults)) {
    cat(" Summary multisite parameters: \n")
    print(x$summaryMultisiteResults, ...)
    cat("\n")
  } else{
    cat(" Multisite results: ")
    cat("No", "\n\n")
  }
  invisible(x)
}
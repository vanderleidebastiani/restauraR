#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
print.simRest <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Overview:\n")
  cat(" Species pool size: ")
  cat(ncol(x$simulation$composition), "\n")
  cat(" Number of simulations: ")
  cat(nrow(x$simulation$composition), "\n")
  cat(" Reference communities: ")
  cat(ifelse(is.null(x$reference), "No", "Yes"), "\n")
  cat(" Supplementary communities: ")
  cat(ifelse(is.null(x$supplementary), "No", "Yes"), "\n")
  cat(" Parameters: ")
  if(!is.null(x$simulation$results)) {
    cat("\n")
    cat(paste("   ",colnames(x$simulation$results)), sep = "\n")
    cat("\n")
  } else{
    cat(ifelse(is.null(x$simulation$results), "No", "Yes"), "\n\n")
  }
  cat("Available results:\n")
  # Avoid collecting the names of the following objects
  if(!is.null(x$simulation$group)) x$simulation$group <- as.matrix(x$simulation$group)
  if(!is.null(x$simulation$results)) x$simulation$results <- as.matrix(x$simulation$results)
  if(!is.null(x$simulation$multifunctionality)) x$simulation$multifunctionality <- as.matrix(x$simulation$multifunctionality)
  if(!is.null(x$reference$results)) x$reference$results <- as.matrix(x$reference$results)
  if(!is.null(x$reference$multifunctionality)) x$reference$multifunctionality <- as.matrix(x$reference$multifunctionality)
  if(!is.null(x$supplementary$results)) x$supplementary$results <- as.matrix(x$supplementary$results)
  if(!is.null(x$supplementary$multifunctionality)) x$supplementary$multifunctionality <- as.matrix(x$supplementary$multifunctionality)
  # Collect list names to print
  cat(collectNames(x, prefix = "..$"), sep = "\n")
  invisible(x)
}
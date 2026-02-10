#' @rdname checkReference
#' @include checkReference.R
#' @encoding UTF-8
#' @export
print.simRestCheck <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Pool parameters: ")
  cat("\n")
  cat(paste(" ", colnames(x$pool$results)), sep = "\n")
  cat("\n")
  cat("Available results:\n")
  # Avoid collecting the names of the following objects
  if(!is.null(x$pool$results)) x$pool$results <- as.matrix(x$pool$results)
  if(!is.null(x$pool$summary)) x$pool$summary <- as.matrix(x$pool$summary)
  if(!is.null(x$reference$results)) x$reference$results <- as.matrix(x$reference$results)
  if(!is.null(x$reference$summary)) x$reference$summary <- as.matrix(x$reference$summary)
  if(!is.null(x$supplementary$results)) x$supplementary$results <- as.matrix(x$supplementary$results)
  # Collect list names to print
  cat(collectNames(x, prefix = "..$"), sep = "\n")
  invisible(x)
}
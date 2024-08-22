#' @rdname checkReference
#' @include checkReference.R
#' @encoding UTF-8
#' @export
print.simRestCheck <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Parameters: ")
  cat("\n")
  cat(paste(" ",colnames(x$pool$results)), sep = "\n")
  cat("\n")
  cat("List of results:\n")
  CollectNames <- function(l, prefix = NULL) {
    if (!is.list(l)) return(NULL)
    names <- Map(paste, names(l), lapply(l, CollectNames), sep = "$")
    names <- gsub("\\$$", "", unlist(names, use.names = FALSE))
    names <- paste(prefix, names, sep = "")
    return(names)
  }
  if(!is.null(x$pool$results)) x$pool$results <- as.matrix(x$pool$results)
  if(!is.null(x$reference$results)) x$reference$results <- as.matrix(x$reference$results)
  if(!is.null(x$supplementary$results)) x$supplementary$results <- as.matrix(x$supplementary$results)
  cat(CollectNames(x, prefix = "..$"), sep = "\n")
  invisible(x)
}
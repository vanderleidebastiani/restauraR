#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
print.simRest <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Informations:\n")
  cat(" Pool size: ")
  cat(ncol(x$simulation$composition), "\n")
  cat(" Number of simulations: ")
  # cat(deparse(nrow(x$simulation$composition)), "\n")
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
  # sprintf("%.3f", eig.temp$p_value) 
  # print(x$simulation$results)
  cat("List of results:\n")
  CollectNames <- function(l, prefix = NULL) {
    if (!is.list(l)) return(NULL)
    names <- Map(paste, names(l), lapply(l, CollectNames), sep = "$")
    names <- gsub("\\$$", "", unlist(names, use.names = FALSE))
    names <- paste(prefix, names, sep = "")
    return(names)
  }
  if(!is.null(x$simulation$group)) x$simulation$group <- as.matrix(x$simulation$group)
  if(!is.null(x$simulation$results)) x$simulation$results <- as.matrix(x$simulation$results)
  if(!is.null(x$simulation$multifunctionality)) x$simulation$multifunctionality <- as.matrix(x$simulation$multifunctionality)
  if(!is.null(x$reference$results)) x$reference$results <- as.matrix(x$reference$results)
  if(!is.null(x$supplementary$results)) x$supplementary$results <- as.matrix(x$supplementary$results)
  cat(CollectNames(x, prefix = "..$"), sep = "\n")
  invisible(x)
}
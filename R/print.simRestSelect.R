#' @rdname selectCommunities
#' @include selectCommunities.R
#' @encoding UTF-8
#' @export
print.simRestSelect <- function(x, ...) {
  cat("Call:\n")
  cat(deparse(x$call), "\n\n")
  cat("Informations:\n")
  cat(" Pool size: ")
  cat(ncol(x$selection$composition), "\n")
  cat(" Number of simulations selected: ")
  cat(nrow(x$selection$composition), "\n")
  cat(" Reference communities: ")
  cat(ifelse(is.null(x$reference), "No", "Yes"), "\n")
  cat(" Supplementary communities: ")
  cat(ifelse(is.null(x$supplementary), "No", "Yes"), "\n")
  cat(" Parameters: ")
  if(!is.null(x$selection$results)) {
    cat("\n")
    cat(paste("   ",colnames(x$selection$results)), sep = "\n")
    cat("\n")
  } else{
    cat(ifelse(is.null(x$selection$results), "No", "Yes"), "\n\n")
  }
  # Mostrar o N?
  # print(x$selection$N)
  cat("List of results:\n")
  CollectNames <- function(l, prefix = NULL) {
    if (!is.list(l)) return(NULL)
    names <- Map(paste, names(l), lapply(l, CollectNames), sep = "$")
    names <- gsub("\\$$", "", unlist(names, use.names = FALSE))
    names <- paste(prefix, names, sep = "")
    return(names)
  }
  if(!is.null(x$selection$group)) x$selection$group <- as.matrix(x$selection$group)
  if(!is.null(x$selection$results)) x$selection$results <- as.matrix(x$selection$results)
  if(!is.null(x$selection$multifunctionality)) x$selection$multifunctionality <- as.matrix(x$selection$multifunctionality)
  if(!is.null(x$reference$results)) x$reference$results <- as.matrix(x$reference$results)
  if(!is.null(x$supplementary$results)) x$supplementary$results <- as.matrix(x$supplementary$results)
  cat(CollectNames(x, prefix = "..$"), sep = "\n")
  invisible(x)
}

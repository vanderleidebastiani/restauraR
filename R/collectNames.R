#' @title Internal function to collect the names of lists.
#' @encoding UTF-8
#' @param x A list object.
#' @param prefix A prefix to include in collected names (default prefix = NULL).
#' @returns A character vector with collected names.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
collectNames <- function(x, prefix = NULL) {
  if (!is.list(x)) return(NULL)
  res <- Map(paste, names(x), lapply(x, collectNames), sep = "$")
  res <- gsub("\\$$", "", unlist(res, use.names = FALSE))
  res <- paste(prefix, res, sep = "")
  return(res)
}
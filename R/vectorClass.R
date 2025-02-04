#' @title Internal function to return the vector class
#' @encoding UTF-8
#' @param x A vector.
#' @returns The vector class. 
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
vectorClass <- function(x){
  res <- class(x)
  # res <- ifelse(res == "integer", "numeric", res)
  # res <- ifelse(any(res == "ordered"), "factor", res)
  if (any(res %in% "ordered")) return("factor")
  if (any(res %in% c("integer", "numeric"))) return("numeric")
  return(res)
}
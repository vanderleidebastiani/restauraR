#' @title Internal function to return the class of a vector
#' @encoding UTF-8
#' @param x A vector.
#' @returns The vector class. 
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
vectorClass <- function(x){
  res <- class(x)
  if (any(res %in% "ordered")) return("factor")
  if (any(res %in% c("integer", "numeric"))) return("numeric")
  return(res)
}
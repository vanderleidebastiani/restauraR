#' @title Internal function to return the vector class.
#' @encoding UTF-8
#' @param x A vector.
#' @returns The vector class. 
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
classSimplerVector <- function(x){
  resClass <- class(x)
  resClass <- ifelse(resClass == "integer", "numeric", resClass)
  resClass <- ifelse(any(resClass == "ordered"), "factor", resClass)
  return(resClass)
}
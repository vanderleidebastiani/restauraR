#' @title Internal function to return only integers in proportional division.
#' @encoding UTF-8
#' @param n A integer to divide.
#' @param prop A numeric vector with proportions.
#' @returns An integer vector with division. The remainder parts are randomly distributed in the groups.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{propMatrix}}
#' @keywords Auxiliary
#' @export
roundDivision <- function(n, prop){
  prop <- prop/sum(prop)
  res <- floor((n*prop))
  resPart <- (n*prop)%%res
  resPart[is.na(resPart)] <- 0
  resPart <- sum(resPart)
  if(sum(res)==0){
    resPart <- n
  }
  if(resPart>0){
    for(i in 1:resPart){
      pos <- sample.int(length(prop), 1)
      res[pos] <- res[pos]+1
    }
  }
  return(res)
}
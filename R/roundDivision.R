#' @title Internal function to divide integers into groups
#' @encoding UTF-8
#' @param n A integer to divide.
#' @param prop A numeric vector with proportions to divide in each group.
#' @returns An integer vector with division. The remainder parts are randomly distributed in each group.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{propMatrix}}
#' @keywords Auxiliary
#' @export
roundDivision <- function(n, prop){
  prop <- prop/sum(prop)
  res <- floor(n*prop)
  remPart <- (n*prop)%%res
  remPart[is.na(remPart)] <- 0
  remPart <- sum(remPart)
  if(sum(res) == 0){
    remPart <- n
  }
  if(remPart > 0){
    for(i in 1:remPart){
      pos <- sample.int(length(prop), 1)
      res[pos] <- res[pos]+1
    }
  }
  return(res)
}
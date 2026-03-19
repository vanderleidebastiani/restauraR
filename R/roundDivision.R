#' @title Internal function to divide an integer into proportional groups
#' @encoding UTF-8
#' @param n An integer to be divided.
#' @param prop A numeric vector with proportions to divide for each group.
#' @returns An integer vector with distribution across groups. Any remaining part is randomly distributed among groups.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{generateCommunityMatrices}}
#' @keywords InternalFunction
roundDivision <- function(n, prop){
  prop <- prop/sum(prop)
  target <- n*prop
  res <- floor(target)
  # remPart <- target%%res
  # remPart[is.na(remPart)] <- 0
  # remPart <- sum(remPart)
  remPart <- n - sum(res)
  # if(sum(res) == 0){
    # remPart <- n
  # }
  if(remPart > 0){
    # for(i in 1:remPart){
    #   pos <- sample.int(length(prop), 1)
    #   res[pos] <- res[pos]+1
    # }
    pos <- sample(length(prop), remPart, replace = TRUE)
    counts <- tabulate(pos, nbins = length(prop))
    res <- res + counts
  }
  return(res)
}
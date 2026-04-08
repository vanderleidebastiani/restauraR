#' @title Internal function to randomly divide an integer into groups
#' @encoding UTF-8
#' @importFrom stats runif
#' @param n An integer to be divided.
#' @param k Number of groups.
#' @returns A numeric vector with the random division.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{generateCommunityMatrices}}
#' @keywords InternalFunction
randDivision <- function(n, k){
  if (k == 1 || n == 0) return(n)
  randNumeric <- stats::runif(n)
  randIntervals <- sort(stats::runif(k-1))
  resTab <- findInterval(randNumeric, randIntervals)
  res <- tabulate(resTab + 1, nbins = k)
  return(res)
}
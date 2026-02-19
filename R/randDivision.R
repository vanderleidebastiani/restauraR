#' @title Internal function to randomly divide an integer into groups
#' @encoding UTF-8
#' @importFrom stats runif
#' @param n An integer to be divided.
#' @param k Number of groups.
#' @returns A numeric vector with the random division.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{generateCommunityMatrices}}
#' @keywords InternalFunction
randDivision <- function(n, k){
  if (k == 1 || n == 0) return(n)
  randNumeric <- stats::runif(n)
  # randIntervals <- sort(stats::runif(k-1, min = min(randNumeric), max = max(randNumeric)))
  randIntervals <- sort(stats::runif(k-1))
  # resTab <- table(findInterval(randNumeric, randIntervals))
  # res <- resTab[as.character(seq.int(from = 0, to = k-1))]
  # res[is.na(res)] <- 0
  # res <- as.numeric(res)
  resTab <- findInterval(randNumeric, randIntervals)
  res <- tabulate(resTab + 1, nbins = k)
  # res <- resTab[as.character(seq.int(from = 0, to = k-1))]
  # res[is.na(res)] <- 0
  # res <- as.numeric(res)
  return(res)
}
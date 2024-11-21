#' @title Internal function to randomly divide a total number into groups.
#' @encoding UTF-8
#' @importFrom stats runif
#' @param n A integer to divide.
#' @param k Number of groups.
#' @returns A numeric vector with the random division.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{propMatrix}}
#' @keywords Auxiliary
#' @export
randDivision <- function(n, k){
  randNumeric <- stats::runif(n)
  randIntervals <- sort(runif(k-1, min = min(randNumeric), max = max(randNumeric)))
  resTab <- table(findInterval(randNumeric, randIntervals))
  res <- resTab[as.character(seq.int(from = 0, to = k-1))]
  res[is.na(res)] <- 0
  res <- as.numeric(res)
  return(res)
}
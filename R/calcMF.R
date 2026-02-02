#' @title Internal function to calculate indices on multifunctionality matrix
#' @description Calculate the landscape and beta multifunctionality indices and average number of functions per site.
#' @encoding UTF-8
#' @param x A data frame with binary multifunctionality tests.
#' @returns A list with the elements:
#' \item{landscape}{Landscape multifunctionality.}
#' \item{beta}{Rao's quadratic entropy calculated on multifunctionality matrix.}
#' \item{average}{Average number of functions per site.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
calcMF <- function(x) {
  # Transform to matrix
  x <- as.matrix(x)
  # Landscape MF
  landscape <- mean(x)
  # Average number of functions per site
  average <- mean(rowSums(x))
  # Beta MF
  # dis <- stats::dist(x)
  dis <- stats::dist(x, method = "binary")
  comm <- rbind(rep(1/nrow(x), nrow(x)))
  colnames(comm) <- rownames(x)
  rownames(comm) <- "combsim"
  beta <- SYNCSA::rao.diversity(comm = comm, phylodist = as.matrix(dis))$PhyRao
  names(beta) <- NULL
  # Score <- sqrt(scales::rescale(landscape) * scales::rescale(beta))
  # picante::raoD(comm)
  # resbiota::calcRAO(x, averages = T)
  # beta <- picante::raoD(x)$beta
  # res <- c(landscape = landscape, beta = beta, average = average)
  res <- list()
  res$landscape <- landscape
  res$beta <- beta
  res$average <- average
  return(res)
}
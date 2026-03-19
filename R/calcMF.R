#' @title Internal function to calculate multifunctionality indices
#' @description Compute multifunctionality indices including landscape-level multifunctionality, beta multifunctionality (based on Rao's quadratic entropy), and the average number of functions per site.
#' @encoding UTF-8
#' @param x A data frame with binary multifunctionality results.
#' @returns A list with the elements:
#' \item{landscape}{Landscape multifunctionality index, representing the overall restoration success across the entire landscape.}
#' \item{beta}{Beta multifunctionality index, calculated as Rao's quadratic entropy on the multifunctionality matrix, quantifying functional diversity among sites.}
#' \item{average}{Average number of restored functions per site.}
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
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
  # restauraR::calcRAO(x, averages = T)
  # beta <- picante::raoD(x)$beta
  # res <- c(landscape = landscape, beta = beta, average = average)
  res <- list()
  res$landscape <- landscape
  res$beta <- beta
  res$average <- average
  return(res)
}
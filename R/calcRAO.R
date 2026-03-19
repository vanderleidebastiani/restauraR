#' @title Internal function to calculate Rao's quadratic entropy (RAO)
#' @description Efficient implementation to calculate the Rao's quadratic entropy (RAO), based on the discomQE function of the adiv package and on the raoD function of the picante package.
#' @encoding UTF-8
#' @param comm A community matrix with species composition in the reference sites. NAs not accepted.
#' @param sppDist Pairwise distance matrix between species (e.g., functional or phylogenetic distances).
#' @param nRef Number of reference sites (the first in the species composition matrix) to calculate among-community diversities only partially. 
#' @param averages Logical argument to specify if return only average for alpha and beta diversities (default averages = FALSE)
#' @returns A matrix with among-community diversities excluding within-community diversity.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
calcRAO <- function(comm, sppDist = NULL, nRef = NULL, averages = FALSE) {
  comm <- as.matrix(comm)
  comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  # nRow <- nrow(comm)
  nCol <- ncol(comm)
  if (!is.null(sppDist)) {
    sppDist <- as.matrix(sppDist)
    matchNames <- match(colnames(comm), colnames(sppDist))
    sppDist <- sppDist[matchNames, matchNames, drop = FALSE]
  } else{
    sppDist <- matrix(1, nCol, nCol)
    diag(sppDist) <- 0
  }
  # Among-community diversity
  commDis <- comm %*% sppDist
  # Within-community diversity
  withinCommDiversity <- rowSums(sweep(comm, 1, commDis, "*", check.margin = FALSE))
  # If averages is equal to FALSE return standard rao
  if(!averages){
    # When nRef is provided calculate amongCommDiversity matrix only partially to performance improvement
    if(!is.null(nRef)) {
      amongCommDiversity <- tcrossprod(commDis, comm[seq.int(nRef),, drop = FALSE]) 
      # Adjustment matrix
      adjustment <- 0.5 * outer(withinCommDiversity, withinCommDiversity[seq.int(nRef)], "+")
    } else{
      amongCommDiversity <- tcrossprod(commDis, comm)  # Equivalent to commDis %*% t(comm)
      # Adjustment matrix
      adjustment <- 0.5 * outer(withinCommDiversity, withinCommDiversity, "+")
    }
    # Among-community diversities excluding within-community diversity
    res <- amongCommDiversity - adjustment
  } else{
    # Else return only average alpha and beta diversities and the total
    total <- apply(comm, 1, sum)
    sampRelAbund <- total/sum(comm)
    xCombined <- apply(comm, 2, sum)/sum(comm)
    res <- list()
    res$total <- sum(sppDist * outer(xCombined, xCombined))
    res$alpha <- sum(withinCommDiversity * sampRelAbund)
    res$beta <- res$total - res$alpha
    res$Fst <- res$beta/res$total
  }
  return(res)
}
#' @title Internal function to calculate Rao's quadratic entropy (RAO)
#' @description Efficient implementation to calculate the Rao's quadratic entropy (RAO), based on the discomQE function of the adiv package and on raoD function of the picante package.
#' @encoding UTF-8
#' @param comm A matrix with species composition in the reference sites. NAs not accepted.
#' @param dis Distances among species.
#' @param nRef Number of reference sites (the first in the species composition matrix) to calculate among-community diversities only partially
#' @param averages Return only average for alpha and beta diversities (default averages = FALSE)
#' @returns A matrix with among-community diversities excluding within-community diversity.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
calcRAO <- function(comm, dis = NULL, nRef = NULL, averages = FALSE) {
  comm <- as.matrix(comm)
  comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  # nRow <- nrow(comm)
  nCol <- ncol(comm)
  if (!is.null(dis)) {
    dis <- as.matrix(dis)
    matchNames <- match(colnames(comm), colnames(dis))
    dis <- dis[matchNames, matchNames, drop = FALSE]
  } else{
    dis <- matrix(1, nCol, nCol)
    diag(dis) <- 0
  }
  # Among-community diversity
  commDis <- comm %*% dis
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
    res$total <- sum(dis * outer(xCombined, xCombined))
    res$alpha <- sum(withinCommDiversity * sampRelAbund)
    res$beta <- res$total - res$alpha
    res$Fst <- res$beta/res$total
  }
  return(res)
}
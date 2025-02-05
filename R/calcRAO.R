#' @title Internal function to calculate Rao's quadratic entropy (RAO)
#' @description Efficient function to calculate the Rao's quadratic entropy (RAO), based on the discomQE function of the adiv package and on raoD function of the picante package.
#' @encoding UTF-8
#' @param comm A matrix with species proportions in the reference sites. NAs not accepted.
#' @param dis Distances among species.
#' @returns A matrix with among-community diversities excluding within-community diversity.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
calcRAO <- function(comm, dis = NULL) {
  comm <- as.matrix(comm)
  comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  nRow <- nrow(comm)
  nCol <- ncol(comm)
  if (!is.null(dis)) {
    dis <- as.matrix(dis)
    match.names <- match(colnames(comm), colnames(dis))
    dis <- dis[match.names, match.names, drop = FALSE]
  } else{
    dis <- matrix(1, nCol, nCol)
    diag(dis) <- 0
  }
  # Among-community diversity
  commDis <- comm %*% dis
  amongCommDiversity <- tcrossprod(commDis, comm)  # Equivalent to commDis %*% t(comm)
  # withinCommDiversity <- rowSums(comm * commDis)
  # Within-community diversity
  withinCommDiversity <- rowSums(sweep(comm, 1, commDis, "*", check.margin = FALSE))
  # Adjustment matrix
  adjustment <- 0.5 * outer(withinCommDiversity, withinCommDiversity, "+")
  # Among-community diversities excluding within-community diversity
  res <- amongCommDiversity - adjustment
  return(res)
}
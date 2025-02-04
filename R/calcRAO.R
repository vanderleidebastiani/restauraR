#' @title Internal function to calculate Rao's quadratic entropy (RAO)
#' @description Rao's quadratic entropy (RAO) is based on the function discomQE from the package adiv.
#' @encoding UTF-8
#' @param comm A matrix with species proportions in the reference sites. NAs not accepted.
#' @param dis Distances among species.
#' @returns A matrix with among-community diversities excluding within-community diversity.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
calcRAO <- function(comm, dis = NULL) { # deep seek
  comm <- as.matrix(comm)
  comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
  nRow <- nrow(comm)
  nCol <- ncol(comm)
  if (!is.null(dis)) {
    dis <- as.matrix(dis)
  } else{
    dis <- matrix(1, nCol, nCol)
    diag(dis) <- 0
  }
  # Calculos vetorizados principais
  comm_dis <- comm %*% dis
  term1 <- tcrossprod(comm_dis, comm)  # Equivalente a comm_dis %*% t(comm)
  deltag <- rowSums(comm * comm_dis)
  # Calculo eficiente da matriz de ajuste
  adjustment <- 0.5 * outer(deltag, deltag, "+")
  # Resultado final
  dg2 <- term1 - adjustment
  return(dg2)
}
# # adiv
# calcRAO <- function (comm, dis = NULL) 
# {
#   comm <- as.matrix(comm)
#   comm <- sweep(comm, 1, rowSums(comm, na.rm = TRUE), "/")
#   nRow <- nrow(comm)
#   nCol <- ncol(comm)
#   if (!is.null(dis)) {
#     dis <- as.matrix(dis)
#   } else{
#     dis <- matrix(1, nCol, nCol)
#     diag(dis) <- 0
#   }
#   deltag <- as.matrix(apply(t(comm), 2, function(x) t(x) %*% dis %*% x))
#   ug <- matrix(1, nRow, 1)
#   dg2 <- comm %*% dis %*% t(comm) - 1/2 * (deltag %*% t(ug) + ug %*% t(deltag))
#   return(dg2)
# }
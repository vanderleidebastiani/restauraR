#' @title Internal function to calculate Community Weighted Variance (CWV)
#' @description The Community Weighted Variance (CWV) is based on the function matrix.t from the package SYNCSA.
#' @encoding UTF-8
#' @importFrom SYNCSA matrix.t
#' @param x A matrix with species proportions in the reference sites. NAs not accepted.
#' @param traitSub Data frame or matrix with species traits. Traits as columns and species as rows.
#' @returns A matrix with Community Weighted Variance.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
calcCWV <- function(x, traitSub){
  temp <- SYNCSA::matrix.t(x, traitSub, scale = FALSE)
  resW <- temp$matrix.w
  resCWM <- temp$matrix.T
  resCWM <- matrix(NA, nrow(resW), ncol(resCWM))
  for(i in 1:nrow(resW)){
    for(j in 1:ncol(resCWM)){
      resCWM[i, j] <- sum(resW[i,]*(traitSub[,j]-resCWM[i,j])^2)
    }
  }
  rownames(resCWM) <- rownames(resCWM)
  colnames(resCWM) <- colnames(resCWM)
  return(resCWM)
}
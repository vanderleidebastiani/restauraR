#' @title Internal function to calculate Community Weighted Variance (CWV)
#' @description Implementation is based on the matrix.t function from the SYNCSA package.
#' @encoding UTF-8
#' @importFrom SYNCSA matrix.t
#' @param comm A community matrix with species composition in the reference sites. NAs not accepted.
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @returns A matrix with Community Weighted Variance.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
calcCWV <- function(comm, traits){
  temp <- SYNCSA::matrix.t(comm, traits, scale = FALSE)
  resW <- temp$matrix.w
  resCWM <- temp$matrix.T
  resCWV <- matrix(NA, nrow(resW), ncol(resCWM))
  for(i in 1:nrow(resW)){
    for(j in 1:ncol(resCWM)){
      resCWV[i, j] <- sum(resW[i,]*(traits[,j]-resCWM[i,j])^2)
    }
  }
  rownames(resCWV) <- rownames(resCWM)
  colnames(resCWV) <- colnames(resCWM)
  return(resCWV)
}
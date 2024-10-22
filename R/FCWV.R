#' @title Internal function to calculate Community Weighted Variance (CWV)
#' @description
#' @encoding UTF-8
#' @importFrom SYNCSA matrix.t
#' @param x A matrix with species proportions in the reference sites. NAs not accepted.
#' @param traitSub Data frame or matrix with species traits. Traits as columns and species as rows.
#' @returns A matrix with Community Weighted Variance.
#' @author See \code{\link{resbiota-package}}.
#' @seealso
#' @references
#' @keywords Auxiliary
#' @export
FCWV <- function(x, traitSub){
  temp <- SYNCSA::matrix.t(x, traitSub, scale = FALSE)
  MW <- temp$matrix.w
  MCWM <- temp$matrix.T
  MCWV <- matrix(NA, nrow(MW), ncol(MCWM))
  for(i in 1:nrow(MW)){
    for(j in 1:ncol(MCWM)){
      MCWV[i, j] <- sum(MW[i,]*(traitSub[,j]-MCWM[i,j])^2)
    }
  }
  rownames(MCWV) <- rownames(MCWM)
  colnames(MCWV) <- colnames(MCWM)
  return(MCWV)
}
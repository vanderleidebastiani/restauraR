


# CMV
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

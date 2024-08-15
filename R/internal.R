# For check function
resSummary <- function(x, props = NULL, ...){
  # x <- x$Height
  # class(x)
  if(!c(inherits(x, what = "numeric") || inherits(x, what = "integer"))){
    res <- data.frame(matrix(NA, nrow = 1, ncol = 4+length(props)))
    if(!is.null(props)) {
      colnames(res) <- c("min", "mean", "median", "max", paste0("quantile_", props))
    } else{
      colnames(res) <- c("min", "mean", "median", "max")
    }  
    return(res)
  } 
  # l1  <- list(min = min(x, na.rm = TRUE),
  #             mean = mean(x, na.rm = TRUE),
  #             median = median(x, na.rm = TRUE),
  #             max = max(x, na.rm = TRUE))
  l1  <- data.frame(min = min(x, na.rm = TRUE),
              mean = mean(x, na.rm = TRUE),
              median = median(x, na.rm = TRUE),
              max = max(x, na.rm = TRUE))
  if(!is.null(props)) {
    # l2 <- as.list(quantile(x, probs = props, ...))
    l2 <- data.frame(t(quantile(x, probs = props)))
    # names(l2) <- paste0("quantile_", props)
    colnames(l2) <- paste0("quantile_", props)
    # res <- as.data.frame(do.call(c, list(l1, l2)))  
    res <- cbind.data.frame(l1, l2)
  } else{
    res <- as.data.frame(l1)
  }
  return(res)
}



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
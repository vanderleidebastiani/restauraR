#' @title Internal function to calculate descriptive statistics
#' @encoding UTF-8
#' @param x A numeric vector to calculate descriptive statistics.
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns A data frame with descriptive statistics: minimum, mean, median, maximum and quantiles
#' @author 
#' @seealso 
#' @keywords Auxiliary
#' @export
resSummary <- function(x, props = NULL, ...){
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

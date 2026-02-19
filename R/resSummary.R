#' @title Internal function to calculate descriptive statistics
#' @encoding UTF-8
#' @importFrom stats median quantile
#' @param x A vector, data frame or matrix to calculate descriptive statistics. The statistics are calculated column-wise in data frame or matrix.
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns A data frame containing the following descriptive statistics: minimum, mean, median, maximum, and requested quantiles.
#' @author See \code{\link{resbiota-package}}.
#' @keywords InternalFunction
#' @export
resSummary <- function(x, props = NULL){
  if(!c(is.atomic(x) || c(inherits(x, what = "data.frame") || inherits(x, what = "matrix")))){
    stop("The x argument must be a vector, data.frame, or matrix")
  }
  # Internal function
  # fResSummary <- function(x, props){
  #   # If non numeric return a data frame with NA
  #   if(!c(inherits(x, what = "numeric") || inherits(x, what = "integer"))){
  #     res <- data.frame(matrix(NA, nrow = 1, ncol = 4+length(props)))
  #     if(!is.null(props)) {
  #       colnames(res) <- c("min", "mean", "median", "max", paste0("quantile_", props))
  #     } else{
  #       colnames(res) <- c("min", "mean", "median", "max")
  #     }  
  #     return(res)
  #   }
  #   # Descriptive statistics
  #   res  <- data.frame(min = min(x, na.rm = TRUE),
  #                      mean = mean(x, na.rm = TRUE),
  #                      median = stats::median(x, na.rm = TRUE),
  #                      max = max(x, na.rm = TRUE))
  #   # Quantiles
  #   if(!is.null(props)) {
  #     d2 <- data.frame(t(stats::quantile(x, probs = props)))
  #     colnames(d2) <- paste0("quantile_", props)
  #     res <- cbind.data.frame(res, d2)
  #   }
  #   return(res)
  # }
  fResSummary <- function(x, props) {
    if (!is.numeric(x) || all(is.na(x))) {
      stats <- rep(NA_real_, 4 + length(props))
      names(stats) <- c("min", "mean", "median", "max", if (!is.null(props)) paste0("quantile_", props))
      return(stats)
    }
    # Descriptive statistics
    minVal <- min(x, na.rm = TRUE)
    meanVal <- mean(x, na.rm = TRUE)
    medianVal <- stats::median(x, na.rm = TRUE)
    maxVal <- max(x, na.rm = TRUE)
    stats <- c(min = minVal, mean = meanVal, median = medianVal, max = maxVal)
    if (!is.null(props)) {
      quantilesVal <- stats::quantile(x, probs = props, na.rm = TRUE, names = FALSE)
      names(quantilesVal) <- paste0("quantile_", props)
      stats <- c(stats, quantilesVal)
    }
    return(stats)
  }
  if(c(inherits(x, what = "data.frame") || inherits(x, what = "matrix"))){
    RES <- lapply(as.data.frame(x), fResSummary, props = props)
    # RES <- do.call(rbind, RES)
    # RES <- t.data.frame(RES)
    RES <- as.data.frame(RES, check.names = FALSE)
  } else {
    # RES <- t.data.frame(fResSummary(x = x, props = props))
    # colnames(RES) <- "Var"
    # RES <- 
    RES <- data.frame(Var = fResSummary(x = x, props = props))
  }
  return(RES)
}
#' @title Internal function to extract names and values of a vector of logic tests
#' @encoding UTF-8
#' @param xvar Character vector specifying the variable name for the x-axis.
#' @param yvar Character vector specifying the variable name for the y-axis.
#' @param logicTest Character vector of logical tests for selection.
#' @returns The \code{extractNamesLogicTest} function returns the order of the variables xvar and yvar in the logical test, while \code{extractValuesLogicTest} returns a list with all numeric thresholds in the logical test. 
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
extractNamesLogicTest <- function(xvar, yvar, logicTest){
  if(!is.character(xvar) || length(xvar)!=1 || is.na(xvar) || nchar(xvar)==0){
    stop("xvar must be a non-null character with a length of 1")
  }
  if(!is.character(yvar) || length(yvar)!=1 || is.na(yvar) || nchar(yvar)==0){
    stop("yvar must be a non-null character with a length of 1")
  }
  if(xvar == yvar){
    stop("xvar and yvar must be different variables")
  }
  if(!is.character(logicTest) || length(logicTest)!=2 || any(is.na(logicTest)) || any(nchar(logicTest) == 0)){
    stop("The logicTest argument must be character of length 2, without NA or empty strings")
  }
  xTest <- grepl(xvar, logicTest, fixed = TRUE)
  yTest <- grepl(yvar, logicTest, fixed = TRUE)
  if(all(!xTest) || all(!yTest)){
    stop("xvar or yvar do not match to any logicTest")
  }
  if(all(xTest == yTest)){
    stop("xvar and yvar match identical positions in logicTest")
  }
  if (xTest[1] && !yTest[1]){
    res <- c(xvar, yvar)
  }
  if (yTest[1] && !xTest[1]){
    res <- c(yvar, xvar)
  }
  if(sum(xTest)>1 || sum(yTest)>1){
    if(all(xTest)){
      if(yTest[1]){
        res <- c(yvar, xvar)  
      } else{
        res <- c(xvar, yvar)  
      }
    } else{
      if(xTest[1]){
        res <- c(xvar, yvar)  
      } else{
        res <- c(yvar, xvar)  
      }
    }
  }
  return(res)
}
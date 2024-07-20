#' @title function to calculate multifunctionality (multCalculation)
#' @description function to calculate multifunctionality
#' @details
#' @encoding UTF-8
#' @importFrom
#' @aliases
#' @param x x
#' @param tests tests
#' @param where where
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
calculateMultifunctionality <- function(x, tests, where = "global"){
  argOptions <- c("selection", "global")
  where <- pmatch(where, argOptions)
  if (length(where) > 1 || any(is.na(where))) {
    stop("Invalid where argument\n")
  }
  if(where == 1){ # if selection
    xPar <- x$selection$results
  } else{
    xPar <- x$simulation$results
  }
  completeString <- paste0('xPar', '$', tests)
  testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text=a))))
  testsSplit <- strsplit(tests, ' ')
  colnames(testsEval) <- sapply(testsSplit, '[', 1)
  testsEval <- as.data.frame(testsEval)
  # testsEval$multifunctionality <- rowSums(testsEval)
  if(where == 1){ # if selection
    x$selection$multifunctionality <- testsEval
    x$selection$results$multifunctionality <- rowSums(testsEval)
  } else{
    x$simulation$multifunctionality <- testsEval
    x$simulation$results$multifunctionality <- rowSums(testsEval)
  }
  return(x)
}
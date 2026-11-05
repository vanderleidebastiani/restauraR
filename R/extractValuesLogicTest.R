#' @rdname extractNamesLogicTest
#' @include extractNamesLogicTest.R
#' @encoding UTF-8
#' @keywords InternalFunction
#' @export
extractValuesLogicTest <- function(logicTest){
  if(!is.character(logicTest) || any(is.na(logicTest)) || any(nchar(logicTest) == 0)){
    stop("The logicTest argument must be character without NA or empty strings")
  }
  pattern <- "(<=|>=|==|!=|<|>)[[:space:]]*([-+]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)?)"
  res <- regmatches(logicTest, gregexpr(pattern, logicTest, perl = FALSE))
  res <- lapply(res, function(x) as.numeric(sub(pattern, "\\2", x, perl = FALSE)))
  return(res)
}

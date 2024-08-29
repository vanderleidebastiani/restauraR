#' @rdname computeParameters
#' @include computeParameters.R
#' @encoding UTF-8
#' @export
computeMultifunctionality <- function(x, tests){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
  } else{
    xPar <- x$selection$results
  }
  completeString <- paste0('xPar', '$', tests)
  testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
  testsSplit <- strsplit(tests, ' ')
  colnames(testsEval) <- sapply(testsSplit, '[', 1)
  testsEval <- as.data.frame(testsEval)
  if(inherits(x, "simRest")){
    x$simulation$multifunctionality <- testsEval
    x$simulation$results$alphamultifunctionality <- rowSums(testsEval)
  } else{
    x$selection$multifunctionality <- testsEval
    x$selection$results$alphamultifunctionality <- rowSums(testsEval)
  }
  return(x)
}
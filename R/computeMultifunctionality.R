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
  # String to select the variable
  completeString <- adjString("xPar", tests)
  # Evaluation
  testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
  if(!inherits(testsEval, "matrix")){
    testsEval <- t(as.matrix(testsEval))  
  }
  colnames(testsEval) <- tests
  # Calculate alphamultifunctionality
  resAlpha <- rowSums(testsEval)
  # Organize the multifunctionality matrix/data.frame
  testsEval <- cbind.data.frame(SIM = xPar$SIM, testsEval)
  if(inherits(x, "simRest")){
    x$simulation$multifunctionality <- testsEval
    x$simulation$results$alphamultifunctionality <- resAlpha
  } else{
    x$selection$multifunctionality <- testsEval
    x$selection$results$alphamultifunctionality <- resAlpha
  }
  # Compute multifunctionality to reference sites
  if(!is.null(x$reference$results)){
    xPar <- x$reference$results
    # Evaluation
    testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
    if(!inherits(testsEval, "matrix")){
      testsEval <- t(as.matrix(testsEval))  
    }
    colnames(testsEval) <- tests
    # Calculate alphamultifunctionality
    resAlpha <- rowSums(testsEval)
    # Organize the multifunctionality matrix/data.frame
    testsEval <- cbind.data.frame(SITE = rownames(xPar), testsEval)
    x$reference$multifunctionality <- testsEval
    x$reference$results$alphamultifunctionality <- resAlpha
  }
  # Compute multifunctionality to supplementary sites
  if(!is.null(x$supplementary$results)){
    xPar <- x$supplementary$results
    # Evaluation
    testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
    if(!inherits(testsEval, "matrix")){
      testsEval <- t(as.matrix(testsEval))  
    }
    colnames(testsEval) <- tests
    # Calculate alphamultifunctionality
    resAlpha <- rowSums(testsEval)
    # Organize the multifunctionality matrix/data.frame
    testsEval <- cbind.data.frame(SITE = rownames(xPar), testsEval)
    x$supplementary$multifunctionality <- testsEval
    x$supplementary$results$alphamultifunctionality <- resAlpha
  }
  return(x)
}
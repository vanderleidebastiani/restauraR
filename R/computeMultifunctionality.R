#' @rdname computeParameters
#' @include computeParameters.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
computeMultifunctionality <- function(x, tests){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
  } else{
    xPar <- x$selection$results
  }
  if(is.null(xPar)){
    stop("The x argument must contain the parameters calculated")
  }
  # String to select the variable
  completeString <- adjString("xPar", tests)
  # Evaluation
  exprs <- lapply(completeString, function(x) parse(text = x, keep.source = FALSE))
  testsEval <- sapply(exprs, function(x) as.numeric(eval(x, envir = list(xPar = xPar))))
  # testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
  if(!inherits(testsEval, "matrix")){
    testsEval <- t(as.matrix(testsEval))  
  }
  colnames(testsEval) <- tests
  # Calculate alphamultifunctionality
  resAlpha <- rowSums(testsEval)
  # Organize the multifunctionality matrix/data.frame
  testsEval <- cbind.data.frame(Simulation = xPar$Simulation, testsEval)
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
    exprs <- lapply(completeString, function(x) parse(text = x, keep.source = FALSE))
    testsEval <- sapply(exprs, function(x) as.numeric(eval(x, envir = list(xPar = xPar))))
    # testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
    if(!inherits(testsEval, "matrix")){
      testsEval <- t(as.matrix(testsEval))  
    }
    colnames(testsEval) <- tests
    # Calculate alphamultifunctionality
    resAlpha <- rowSums(testsEval)
    # Organize the multifunctionality matrix/data.frame
    testsEval <- cbind.data.frame(Site = rownames(xPar), testsEval)
    x$reference$multifunctionality <- testsEval
    x$reference$results$alphamultifunctionality <- resAlpha
  }
  # Compute multifunctionality to supplementary sites
  if(!is.null(x$supplementary$results)){
    xPar <- x$supplementary$results
    # Evaluation
    exprs <- lapply(completeString, function(x) parse(text = x, keep.source = FALSE))
    testsEval <- sapply(exprs, function(x) as.numeric(eval(x, envir = list(xPar = xPar))))
    # testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text = a))))
    if(!inherits(testsEval, "matrix")){
      testsEval <- t(as.matrix(testsEval))  
    }
    colnames(testsEval) <- tests
    # Calculate alphamultifunctionality
    resAlpha <- rowSums(testsEval)
    # Organize the multifunctionality matrix/data.frame
    testsEval <- cbind.data.frame(Site = rownames(xPar), testsEval)
    x$supplementary$multifunctionality <- testsEval
    x$supplementary$results$alphamultifunctionality <- resAlpha
  }
  return(x)
}
#' @title function to select communities
#' @description select simulated communities based on tests provided and add information to x
#' @details
#' @encoding UTF-8
#' @importFrom
#' @aliases
#' @param param data.frame with parameters of simulated communities
#' @param comp data.frame with compositions of simulated communities
#' @param tests list with tests to be performed
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
comSelection <- function(x, tests, where = "global"){
  # param, comp
  if(where == "global"){
    xPar <- x$sim$results
    composition <- x$sim$composition
  } else{
    xPar <- x$selection$results
    composition <- x$selection$composition
  }
	xPar <- as.data.frame(xPar) # Force to data.frame
	completeString <- paste0('xPar', '$', tests)
	testsEval <- sapply(completeString, function(a) eval(parse(text=a)))
	pos <- apply(testsEval, 1, all) 
	selPar <- xPar[pos,] 
	# selCom <- comp[pos,]
	selCom <- composition[pos,]
	
	#number of selected communities:
	nSel <- apply(testsEval, 2, sum)
	names(nSel) <- tests
	nSel <- c(nSel, all = sum(pos))
	
	#thresholds:
	testsSplit <- strsplit(tests, ' ')
	# trsh <- as.numeric(sapply(testsSplit, '[', 3))
	trsh <- sapply(testsSplit, '[', 3)
	names(trsh) <- sapply(testsSplit, '[', 1)
	# outSel <- list(parameters = selPar,
	# 			   composition = selCom,
	# 			   N = nSel,
	# 			   thresholds = trsh)
	x$selection$results <- selPar
	x$selection$composition <- selCom
	x$selection$N <- nSel
	x$selection$thresholds <- trsh
	return(x)
}
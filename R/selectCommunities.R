#' @title function to select communities (comSelection)
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
selectCommunities <- function(x, tests, where = "global"){
  argOptions <- c("selection", "global")
  where <- pmatch(where, argOptions)
  if (length(where) > 1 || any(is.na(where))) {
    stop("Invalid where argument\n")
  }
  # Get parameters and composition
  if(where == 1){ # if selection
    xPar <- x$selection$results
    comp <- x$selection$composition
    group <- x$selection$group
  } else{
    xPar <- x$simulation$results
    comp <- x$simulation$composition
    group <- x$simulation$group
  }
  # Evaluate test
	completeString <- paste0('xPar', '$', tests)
	testsEval <- sapply(completeString, function(a) eval(parse(text=a)))
	pos <- apply(testsEval, 1, all) 
	# Select 
	selPar <- xPar[pos, , drop = FALSE] 
	selCom <- comp[pos, , drop = FALSE]
	selGroup <- group[pos, , drop = FALSE]
	# Number of selected communities
	nSel <- apply(testsEval, 2, sum)
	names(nSel) <- tests
	nSel <- c(nSel, all = sum(pos))
	# Format thresholds
	testsSplit <- strsplit(tests, ' ')
	trsh <- sapply(testsSplit, '[', 3)
	names(trsh) <- sapply(testsSplit, '[', 1)
	# Set results
	x$selection$results <- selPar
	x$selection$composition <- selCom
	x$selection$group <- group
	x$selection$N <- nSel
	x$selection$thresholds <- trsh
	return(x)
}
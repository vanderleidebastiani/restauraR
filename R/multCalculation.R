#' @title function to calculate multifunctionality
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
multCalculation <- function(x, tests, where = "global"){
  
  # x, th, bel
  # x list of table with parameters of different areas
  # th named vector of thresholds. Names must match x columns
  # bel character vector indicating functions that must be below threshold. Must match x columns.
 
  # x <- selSim$selection$results
  # th <- apply(x[c(3,4,5)], 2, mean)
  # bel <- "richness"
  
  # funs <- names(th) #functions
  # if(!missing(bel)){
  #   th[bel] <- -th[bel]
  #   bel2 <- bel #para usar no apply
  # } #reflect functions
  # x2_nrow <- NULL
  # e <- environment() #para buscar bel2, abaixo
  # x2 <- lapply(x, FUN = function(y){ #for each table:
  #   if(exists('bel2', envir = e, inherits = FALSE)){
  #     y[,bel2] <- -y[,bel2]
  #   } #reflect functions. Nota sobre exists: procura no environment definido em 'envir', no caso, environment da funcao multCalculation. Porem, considera que 'bel' existe mesmo se nao tiver valor atribuido, por que aparece no environment como 'missing argument'. Por isso, preciso salvar bel em bel2, acima. missing() ou hasArg() nao funcionam pq buscam apenas no environment da funcao anonima FUN. 
  #   x3 <- apply(y, 1, FUN = function(y2){ #for each line
  #     tests <- as.numeric(y2[funs] > th) #is function above threshold?
  #     mult <- sum(tests) #sum of functions above threshold
  #     return(c(tests, mult)) #output
  #   })
  #   x3 <- t(x3) #transpose
  #   colnames(x3) <- c(funs, 'multifunctionality') #rename
  #   return(x3)
  #   # x2_nrow <- c(x2_nrow, nrow(x3))
  # })
  # return(x2)
  if(where == "global"){
    xPar <- x$sim$results
  } else{
    xPar <- x$selection$results
  }
  xPar <- as.data.frame(xPar) # Force to data.frame
  completeString <- paste0('xPar', '$', tests)
  testsEval <- sapply(completeString, function(a) as.numeric(eval(parse(text=a))))
  testsSplit <- strsplit(tests, ' ')
  colnames(testsEval) <- sapply(testsSplit, '[', 1)
  testsEval <- as.data.frame(testsEval)
  testsEval$mult <- rowSums(testsEval)
  if(where == "global"){
    x$sim$multifunctionality <- testsEval
  } else{
    x$selection$multifunctionality <- testsEval
  }
  return(x)
}
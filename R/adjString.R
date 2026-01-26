#' @title Internal function to format a vector of logical expressions for evaluation within a data frame context
#' @encoding UTF-8
#' @param prefix Character string specifying the name of the data frame object.
#' @param logicTest The vector containing logical expressions to be evaluated.
#' @returns The vector of formatted logical tests.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{selectCommunities}}
#' @keywords Auxiliary
#' @export
adjString <- function(prefix, logicTest){
  res <- vector("character", length = length(logicTest))
  for(k in 1:length(logicTest)){
    multipleTests <- strsplit(logicTest[k], "&|\\|")[[1]]
    if(length(multipleTests)>1){
      # Remove whitespace
      multipleTests <- trimws(multipleTests, which = "both")
      if(grepl("&", logicTest[k])){
        res[k] <- paste(paste0(prefix, "$", multipleTests), collapse = " & ")  
      } else{ # grepl("\\|", logicTest[k])
        res[k] <- paste(paste0(prefix, "$", multipleTests), collapse = " | ")  
      }
    } else{
      res[k] <- paste0(prefix, "$", logicTest[k])  
    }
  }
  return(res)
}
#' @title Internal function to adjust a vector of logical tests to be evaluate on a data.frame
#' @encoding UTF-8
#' @param prefix The name of data.frame object.
#' @param logicTest The vector of logical tests.
#' @returns The vector of logical tests adjusted.
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
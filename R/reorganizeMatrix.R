#' @title Internal function to standardize matrix structures
#' @description \code{makeMatrixTemplate} Creates an empty template matrix. This function concatenates two or more data.frame (or matrix) 
#' and return an empty data.table object (template) with 0 rows and columns equal to all unique columns of the input objects. 
#' 
#' \code{reorganizeMatrix} rearranges one matrix following a template layout. The names of the columns are required to reorganise the matrices.
#' @encoding UTF-8
#' @aliases makeMatrixTemplate
#' @importFrom data.table as.data.table rbindlist
#' @param template A template matrix returned by \code{makeMatrixTemplate} function to guide reorganization.
#' @param x A data.frame or matrix to be reorganized.
#' @param fillNA Logical argument (TRUE or FALSE) to specify if missing cells (NA) should be replaced with zeros (default fillNA = FALSE).
#' @param ... Objects of class data.frame (or matrix) to be concatenated.
#' @returns The \code{makeMatrixTemplate} function returns an empty data.table object with a reference structure, while \code{reorganizeMatrix} aligns existing data to that structure and returns the reorganised matrix.
#' @author See \code{\link{resbiota-package}}.
#' @keywords Auxiliary
#' @export
reorganizeMatrix <- function(template, x, fillNA = FALSE){
  rowNameX <- rownames(x)
  template <- data.table::as.data.table(template[0,, drop = FALSE])
  x <- data.table::as.data.table(x, keep.rownames = FALSE)
  x <- data.table::rbindlist(list(template, x), use.names = TRUE, fill = TRUE)
  x <- as.matrix(x)
  rownames(x) <- rowNameX
  if(fillNA){
    x[is.na(x)] <- 0
  }
  return(x)
}
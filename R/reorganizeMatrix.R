#' @title reorganizeMatrix
#' @description
#' @details
#' @encoding UTF-8
#' @aliases makeMatrixTemplate
#' @param template 
#' @param x 
#' @param fillNA 
#' @param ...
#' @returns 
#' @author 
#' @seealso
#' @references
#' @keywords Auxiliary
#' @examples
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
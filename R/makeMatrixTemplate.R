#' @rdname rearrangementMatrix
#' @include rearrangementMatrix.R
#' @encoding UTF-8
#' @export
makeMatrixTemplate <- function(...){
  ARGS <- list(...)
  ARGS <- lapply(ARGS, function(x) x[0, , drop = FALSE])
  res <- lapply(ARGS, data.table::as.data.table, keep.rownames = FALSE)
  res <- data.table::rbindlist(res, use.names = TRUE, fill = TRUE)
  return(res)
}
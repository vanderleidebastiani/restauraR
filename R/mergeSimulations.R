#' @title 
#' @description 
#' @details
#' @encoding UTF-8
#' @importFrom data.table rbindlist as.data.table
#' @aliases
#' @param 
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
mergeSimulations <- function(...) {
  RES <- vector("list")
  ARGS <- list(...)
  # Group
  group <- lapply(ARGS, function(x) x$simulation$group)
  group <- lapply(group, as.data.table, keep.rownames = FALSE)
  group <- data.table::rbindlist(group, use.names = TRUE, fill = TRUE)
  RES$simulation$group <- data.frame(group)
  # Composition
  comp <- lapply(ARGS, function(x) x$simulation$composition)
  comp <- lapply(comp, as.data.table, keep.rownames = TRUE)
  comp <- data.table::rbindlist(comp, use.names = TRUE, fill = TRUE)
  resRowNames <- comp$rn
  comp <- as.matrix(comp[,-1])
  rownames(comp) <- resRowNames
  comp[is.na(comp)] <- 0
  RES$simulation$composition <- comp
  class(RES) <- "simRest"
  return(RES)
}

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
  # ref = NULL
  RES <- vector("list")
  # if(is.null(ref)){
  #   argg <- list(...)
  # } else{
  #   argg <- c(as.list(environment()), list(...))
  # }
  # f1 <- function(x) {
  #   if(inherits(x, "list")) {
  #     res <- x$sim$composition
  #   } else {
  #     res <- x/rowSums(x) # Force reference composition to proportions
  #     # res <- x
  #   }
  #   return(res)
  # }
  argg <- list(...)
  argg <- lapply(argg, function(x) x$sim$composition)
  argg <- lapply(argg, as.data.table, keep.rownames = TRUE)
  comp <- data.table::rbindlist(argg, use.names = TRUE, fill = TRUE)
  resRowNames <- comp$rn
  comp <- as.matrix(comp[,-1])
  rownames(comp) <- resRowNames
  comp[is.na(comp)] <- 0
  RES$sim$composition <- comp
  return(RES)
}

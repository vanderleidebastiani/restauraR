#' @title checkReference
#' @description checkReference
#' @details
#' @encoding UTF-8
#' @aliases
#' @param reference description
#' @param trait description
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
checkReference <- function(reference, trait, cwm, cwv, rao, stan, supplementary = NULL){
  RES <- list()
  # Create list to use in calculateParameters
  x <- list()
  x$simulation$composition <- reference
  x$simulation$group <- data.frame(NAMES = rownames(reference))
  resPar <- calculateParameters(x, trait = trait, cwm = cwm, cwv = cwv, rao = rao, stan = stan, supplementary = supplementary)
  RES$reference$results <- resPar$sim$results[ , -1, drop = FALSE]
  RES$reference$summary <- summary(RES$reference$results)
  if(!is.null(supplementary)){
    RES$supplementary$results <- resPar$supplementary$results
    RES$supplementary$summary <- summary(RES$supplementary$results)
  }
  return(RES)
}

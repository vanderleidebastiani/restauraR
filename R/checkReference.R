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
checkReference <- function(reference, trait, cwm, cwv, rao, stan, supplementary = NULL, props = NULL){
  # RES <- list()
  RES <- list(call = match.call())
  # Create list to use in calculateParameters
  x <- list()
  x$simulation$composition <- reference
  x$simulation$group <- data.frame(NAMES = rownames(reference))
  maxCom <- matrix(1, nrow = 1, ncol = nrow(trait))
  rownames(maxCom) <- "maxCom"
  colnames(maxCom) <- rownames(trait)
  resPar <- calculateParameters(x, trait = trait, cwm = cwm, cwv = cwv, rao = rao, stan = stan, reference = maxCom, supplementary = supplementary)
  RES$pool$results <- resPar$reference$results
  # RES$pool$summary <- summary(trait)
  # RES$pool$summary <- sapply(trait, resSummary, props = props)
  resSum <- lapply(trait, resSummary, props = props)
  resSum <- data.table::rbindlist(resSum)
  resSum <- as.data.frame(resSum)
  resSum <- t.data.frame(resSum)
  colnames(resSum) <- colnames(trait)
  RES$pool$summary <- resSum
  RES$reference$results <- resPar$simulation$results[ , -1, drop = FALSE]
  # RES$reference$summary <- summary(RES$reference$results)
  # RES$reference$summary <- sapply(RES$reference$results, resSummary, props = props)
  resSum <- lapply(RES$reference$results, resSummary, props = props)
  resSum <- data.table::rbindlist(resSum)
  resSum <- as.data.frame(resSum)
  resSum <- t.data.frame(resSum)
  colnames(resSum) <- colnames(RES$reference$results)
  RES$reference$summary <- resSum
  if(!is.null(supplementary)){
    RES$supplementary$results <- resPar$supplementary$results
    # RES$supplementary$summary <- summary(RES$supplementary$results)
    # RES$supplementary$summary <- sapply(RES$supplementary$results, resSummary, props = props)
    resSum <- lapply(RES$supplementary$results, resSummary, props = props)
    resSum <- data.table::rbindlist(resSum)
    resSum <- as.data.frame(resSum)
    resSum <- t.data.frame(resSum)
    colnames(resSum) <- colnames(RES$supplementary$results)
    RES$supplementary$summary <- resSum
  }
  class(RES) <- "simRestCheck"
  return(RES)
}

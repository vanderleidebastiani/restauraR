#' @title checkReference
#' @description checkReference
#' @details
#' @encoding UTF-8
#' @aliases print.simRestCheck
#' @param reference A matrix with species proportions in the reference sites. NAs not accepted.
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param cwm A vector with traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param cwv A vector with traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist).
#' @param stan A vector with parameters names to specify which parameters should be standardized by the maximum.
#' @param supplementary A matrix with species proportions in the supplementary sites. NAs not accepted. (default supplementary = NULL).
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns 
#' @author 
#' @seealso
#' @references
#' @keywords MainFunction
#' @examples
#' @export
checkReference <- function(reference, trait, cwm, cwv, rao, stan, supplementary = NULL, props = NULL){
  RES <- list(call = match.call())
  # Create list, simRest class, to use in computeParameters
  x <- list()
  x$simulation$composition <- reference
  x$simulation$group <- data.frame(NAMES = rownames(reference))
  class(x) <- "simRest"
  maxCom <- matrix(1, nrow = 1, ncol = nrow(trait))
  rownames(maxCom) <- "Pool"
  colnames(maxCom) <- rownames(trait)
  resPar <- computeParameters(x, trait = trait, cwm = cwm, cwv = cwv, rao = rao, stan = stan, reference = maxCom, supplementary = supplementary)
  RES$pool$results <- resPar$reference$results
  RES$pool$summary <- resSummary(trait, props = props)
  RES$reference$results <- resPar$simulation$results[ , -1, drop = FALSE]
  RES$reference$summary <- resSummary(RES$reference$results, props = props)
  if(!is.null(supplementary)){
    RES$supplementary$results <- resPar$supplementary$results
    RES$supplementary$summary <- resSummary(RES$supplementary$results, props = props)
  }
  class(RES) <- "simRestCheck"
  return(RES)
}
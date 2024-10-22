#' @title Check species pool and functional parameters in reference sites
#' @description Calculate basic parameters in reference community: richness, Community Weighted Mean, Community Weighted Variance and Rao Quadratic Entropy. 
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
#' @param x Objects of class "simRestCheck" to print.
#' @param ... Additional arguments for respective methods.
#' @returns A list (class `simRestCheck`) with the elements:
#' \item{call}{The arguments used.}
#' \item{pool$results}{A data frame with calculated parameters in species pool.}
#' \item{pool$summary}{A data frame with the descriptive statistics of the species pool.}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{reference$summary}{A data frame with the descriptive statistics of calculated parameters in reference sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' \item{supplementary$summary}{A data frame with the descriptive statistics of calculated parameters in supplementary sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}},
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' checkReference(reference = cerrado.mini$reference,
#'                trait = cerrado.mini$traits,
#'                cwm = "BT",
#'                rao = c("SLA", "Height", "Seed"),
#'                props = c(0.75, 0.9))
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
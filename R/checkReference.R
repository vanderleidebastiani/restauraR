#' @title Check species pool and functional parameters in reference sites
#' @description Calculate ecological parameters in reference communities: richness, Community Weighted Mean, Community Weighted Variance and Rao's Quadratic Entropy. 
#' @encoding UTF-8
#' @aliases print.simRestCheck
#' @param reference Matrix with species composition in the reference sites. NAs not accepted.
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param cwm Character vector specifying traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param cwv Character vector specifying traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.
#' @param rao Character vector specifying trait names to calculate Rao's Quadratic Entropy, or distance matrix (class dist). This argument can be a list to calculate multiple Rao indices using different trait sets or species distance matrices.
#' @param supplementary Matrix with species composition in the supplementary sites. NAs not accepted. (default supplementary = NULL).
#' @param props Numeric vector of probabilities with values between 0 and 1 to produce sample quantiles corresponding to the given probabilities (default props = NULL).
#' @param x An object of class "simRestCheck" to print.
#' @param ... Additional arguments for respective methods.
#' @returns A list (class "simRestCheck") with the elements:
#' \item{call}{The arguments used.}
#' \item{pool$results}{Data frame with calculated parameters for species pool.}
#' \item{pool$summary}{Data frame with the descriptive statistics of the species pool.}
#' \item{reference$results}{Data frame with calculated parameters for reference sites.}
#' \item{reference$summary}{Data frame with the descriptive statistics of calculated parameters for reference sites.}
#' \item{supplementary$results}{Data frame with calculated parameters for supplementary sites.}
#' \item{supplementary$summary}{Data frame with the descriptive statistics of calculated parameters for supplementary sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}},
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' checkReference(reference = cerrado.mini$reference,
#'                traits = cerrado.mini$traits,
#'                cwm = "BT",
#'                rao = c("SLA", "Height", "Seed"),
#'                props = c(0.75, 0.9))
#' @export
checkReference <- function(reference, traits, cwm = NULL, cwv = NULL, rao = NULL, supplementary = NULL, props = NULL){
  RES <- list(call = match.call())
  # Check if all number are integer
  anyPositive <- length(reference[reference>0])>0
  allInteger <- all(reference%%1 == 0)
  rowCheck <- isTRUE(all.equal(rowSums(reference), rep(1, nrow(reference)), check.attributes = FALSE, check.class = FALSE))
  if(anyPositive && !allInteger && !rowCheck){
    stop("Reference with species proportions must sum to 1 for each site")
  }
  # Create list, simRest class, to use in computeParameters
  x <- list()
  x$simulation$composition <- reference
  x$simulation$group <- data.frame(NAMES = rownames(reference))
  class(x) <- "simRest"
  if(!allInteger){
    maxCom <- matrix(1/nrow(traits), nrow = 1, ncol = nrow(traits)) 
  } else{
    maxCom <- matrix(1, nrow = 1, ncol = nrow(traits))  
  }
  rownames(maxCom) <- "Pool"
  colnames(maxCom) <- rownames(traits)
  resPar <- computeParameters(x, traits = traits, cwm = cwm, cwv = cwv, rao = rao, reference = maxCom, supplementary = supplementary)
  RES$pool$results <- resPar$reference$results
  RES$pool$summary <- resSummary(traits, props = props)
  RES$reference$results <- resPar$simulation$results[ , -1, drop = FALSE]
  RES$reference$summary <- resSummary(RES$reference$results, props = props)
  if(!is.null(supplementary)){
    RES$supplementary$results <- resPar$supplementary$results
    RES$supplementary$summary <- resSummary(RES$supplementary$results, props = props)
  }
  class(RES) <- "simRestCheck"
  return(RES)
}
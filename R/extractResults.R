#' @title Extract results
#' @description Help function to easily extract results. Most methods are default R list extractions. However, the option "simUnavailableSpecies" (type argument) checks the unavailable species present in selected communities these are the species necessary to achieve the thresholds when selected communities have at least one species not available on the market.
#' @encoding UTF-8
#' @param x A object of class "simRest" or "simRestSelect" to extract the results.
#' @param type A option to extract results, partial match to "simComposition", "simResults", "simMultifunctionality", "simUnavailableSpecies", "refComposition", "refResults", "refMultifunctionality", "supComposition", "supResults", "supMultifunctionality". See Value below.
#' @param dbFormat The logical argument to specify if return species composition in database format, a data.frame with three columns: "Site", "Species", "Abundance". 
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @returns A data.frame or matrix according to the chosen method:
#' \item{simComposition}{A matrix with species composition for simulated communities.}
#' \item{simResults}{A data frame with calculated parameters in each simulated community.}
#' \item{simMultifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{simUnavailableSpecies}{A data frame with the species list to achieve the specified thresholds.}
#' \item{refComposition}{A matrix with species composition for reference sites.}
#' \item{refResults}{A data frame with calculated parameters in reference sites.}
#' \item{refMultifunctionality}{A data frame with binary multifunctionality tests for reference sites.}
#' \item{supComposition}{A matrix with species composition for supplementary sites.}
#' \item{supResults}{A data frame with calculated parameters in supplementary sites.}
#' \item{supMultifunctionality}{A data frame with binary multifunctionality tests for reference sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}}, 
#' \code{\link{viewResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @export
extractResults <- function(x, type = "simResults", dbFormat = FALSE, trait = NULL, ava = NULL){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  typeMETHOD <- c("simComposition",
                  "simResults", 
                  "simMultifunctionality",
                  "simUnavailableSpecies", 
                  "refComposition",
                  "refResults",
                  "refMultifunctionality",
                  "supComposition",
                  "supResults",
                  "supMultifunctionality")
  typeTemp <- pmatch(type, typeMETHOD)
  if (length(typeTemp) > 1) {
    stop("Only one argument is accepted in type")
  }
  if (is.na(typeTemp)) {
    stop("Invalid type")
  }
  type <- match.arg(type, typeMETHOD)
  switch(type, 
         simComposition = {
           if(inherits(x, "simRest")){
             res <- x$simulation$composition
           } else{
             res <- x$selection$composition
           }
         },
         simResults = {
           if(inherits(x, "simRest")){
             res <- x$simulation$results
           } else{
             res <- x$selection$results
           }
         },
         simMultifunctionality = {
           if(inherits(x, "simRest")){
             res <- x$simulation$multifunctionality  
           } else{
             res <- x$selection$multifunctionality
           }
         },
         simUnavailableSpecies = {
           if(is.null(trait) || is.null(ava)){
             stop("simUnavailableSpecies method require the trait data and the ava argument")
           }
           if(inherits(x, "simRest")){
             sppSelected <- colSums(x$simulation$composition, na.rm = TRUE) 
           } else{
             sppSelected <- colSums(x$selection$composition, na.rm = TRUE) 
           }
           sppSelected <- names(sppSelected[sppSelected>0])
           traitsNames <- colnames(trait)
           if(!inherits(ava, 'character') || !all(ava %in% traitsNames) || length(ava)>1){
             stop("ava must be a character indicating a single column of the trait data frame")
           }
           unaSpp <- rownames(trait)[!as.logical(trait[,ava])]
           res <- data.frame(UnavailableSpp = intersect(unaSpp, sppSelected))
         },
         refComposition = {
           res <- x$reference$composition
         },
         refResults = {
           res <- x$reference$results
         },
         refMultifunctionality = {
           res <- x$reference$multifunctionality
         },
         supComposition = {
           res <- x$supplementary$composition
         },
         supResults = {
           res <- x$supplementary$results
         },
         supMultifunctionality = {
           res <- x$supplementary$multifunctionality
         }
  )
  if(type %in% c("simComposition", "refComposition", "supComposition") && dbFormat && !is.null(res)){
    res <- data.frame(expand.grid(rownames(res), colnames(res)), as.vector(res))
    res <- res[(res[, 3] > 0) & !is.na(res[, 3]), , drop = FALSE]
    res <- res[sort.list(res[, 1]), , drop = FALSE]
    colnames(res) <- c("Site", "Species", "Abundance")
  }
  return(res)
}
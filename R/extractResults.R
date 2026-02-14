#' @title Extract components of simulation results
#' @description Helper function to easily extract specific components from simulation results. Most extraction methods use standard R list indexing. The option "simUnavailableSpecies" (type argument) identifies unavailable species present in selected communities, that is, the species that are necessary to achieve the thresholds but are not available on the market.
#' @encoding UTF-8
#' @param x An object of class "simRest" or "simRestSelect" to extract the results.
#' @param type An option to extract results, partial match to "simComposition", "simGroup", "simBaseline", "simAdditions", "simResults", "simMultifunctionality", "simMultisiteResults", "simMultisiteCombinations", "simUnavailableSpecies", "refComposition", "refResults", "refMultifunctionality", "supComposition", "supResults", "supMultifunctionality". See value below.
#' @param dbFormat Logical argument to specify if return species composition in database format, a data.frame with three columns: "Site", "Species", "Abundance". 
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava Character vector specifying the trait name that indicates the availability of species in traits data (binary: 1 = available, 0 = unavailable).
#' @returns A data.frame or matrix according to the chosen method:
#' \item{simComposition}{Matrix with species composition for simulated communities.}
#' \item{simGroup}{Data frame with complementary information for simulated sites.}
#' \item{simBaseline}{Matrix with baseline species composition for simulated communities.}
#' \item{simAdditions}{Matrix with species additions for simulated communities.}
#' \item{simResults}{Data frame with calculated parameters in each simulated community.}
#' \item{simMultifunctionality}{Data frame with binary multifunctionality tests.}
#' \item{simMultisiteResults}{Data frame with calculated parameters in each communities combinations.}
#' \item{simMultisiteCombinations}{Matrix with binary communities combinations.}
#' \item{simUnavailableSpecies}{Data frame with the species list to achieve the specified thresholds.}
#' \item{refComposition}{Matrix with species composition for reference sites.}
#' \item{refResults}{Data frame with calculated parameters in reference sites.}
#' \item{refMultifunctionality}{Data frame with binary multifunctionality tests for reference sites.}
#' \item{supComposition}{Matrix with species composition for supplementary sites.}
#' \item{supResults}{Data frame with calculated parameters in supplementary sites.}
#' \item{supMultifunctionality}{Data frame with binary multifunctionality tests for supplementary sites.}
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
extractResults <- function(x, type = "simResults", dbFormat = FALSE, traits = NULL, ava = NULL){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  typeMETHOD <- c("simComposition",
                  "simGroup", 
                  "simBaseline", 
                  "simAdditions",
                  "simResults", 
                  "simMultifunctionality",
                  "simMultisiteResults",
                  "simMultisiteCombinations",
                  "simUnavailableSpecies", 
                  "refComposition",
                  "refResults",
                  "refMultifunctionality",
                  "supComposition",
                  "supResults",
                  "supMultifunctionality")
  typeTemp <- pmatch(type, typeMETHOD)
  if (length(typeTemp) > 1) {
    stop("Only one type can be specified")
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
         simGroup = {
           if(inherits(x, "simRest")){
             res <- x$simulation$group
           } else{
             res <- x$selection$group
           }
         },
         simBaseline = {
           if(inherits(x, "simRest")){
             res <- x$simulation$baseline
           } else{
             res <- x$selection$baseline
           }
         },
         simAdditions = {
           if(inherits(x, "simRest")){
             # Extract composition
             comp <- x$simulation$composition
             # Check if all number are integer
             allInteger <- all(comp%%1 == 0)
             # Extract baseline
             baseline <- x$simulation$baseline
             # Calculate additions
             # If proportions
             if(!allInteger){
               res <- (comp*2) - baseline
             } else{ # If counts
               res <- comp - baseline
             }
           } else{
             # Extract composition
             comp <- x$selection$composition
             # Check if all number are integer
             allInteger <- all(comp%%1 == 0)
             # Extract baseline
             baseline <- x$selection$baseline
             # Calculate additions
             # If proportions
             if(!allInteger){
               res <- (comp*2) - baseline
             } else{ # If counts
               res <- comp - baseline
             }
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
         simMultisiteResults = {
           if(inherits(x, "simRest")){
             res <- x$simulation$multisite$results
           } else{
             res <- x$selection$multisite$results
           }
         },
         simMultisiteCombinations = {
           if(inherits(x, "simRest")){
             res <- x$simulation$multisite$combinations
           } else{
             res <- x$selection$multisite$combinations
           }
         },
         simUnavailableSpecies = {
           if(is.null(traits) || is.null(ava)){
             stop("The simUnavailableSpecies type requires both traits data and the ava argument")
           }
           if(inherits(x, "simRest")){
             sppSelected <- colSums(x$simulation$composition, na.rm = TRUE) 
           } else{
             sppSelected <- colSums(x$selection$composition, na.rm = TRUE) 
           }
           sppSelected <- names(sppSelected[sppSelected>0])
           traitsNames <- colnames(traits)
           if(!inherits(ava, "character") || !all(ava %in% traitsNames) || length(ava)>1){
             stop("The ava argument must be a character vector specifying a single columm from the traits data frame")
           }
           unaSpp <- rownames(traits)[!as.logical(traits[,ava])]
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
  if(type %in% c("simComposition", "simBaseline", "simAdditions", "refComposition", "supComposition") && dbFormat && !is.null(res)){
    res <- data.frame(expand.grid(rownames(res), colnames(res)), as.vector(res))
    res <- res[(res[, 3] > 0) & !is.na(res[, 3]), , drop = FALSE]
    res <- res[sort.list(res[, 1]), , drop = FALSE]
    colnames(res) <- c("Site", "Species", "Abundance")
  }
  return(res)
}
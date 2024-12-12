#' @title extractResults
#' @description Check the unavailable species that were present in selected communities these are the species necessary to achieve the thresholds 
#' when selected communities have at least one species not available on the market.
#' @encoding UTF-8
#' @param x A object of class "simRest" or "simRestSelect" to visualize results
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param type unavailableSpecies
#' @returns 
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
#' @examples
#' @export
extractResults <- function(x, type = "simResults", dbFormat = FALSE, trait = NULL, ava = NULL){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  typeMETHOD <- c("simComposition", # 1
                  # "simGroups", # 2
                  "simResults", # 3
                  "simMultifunctionality", # 4
                  "simUnavailableSpecies", # 5
                  "refComposition", # 6
                  "refResults", # 7
                  "refMultifunctionality", # 8
                  "supComposition", # 9
                  "supResults", # 10
                  "supMultifunctionality") # 11
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
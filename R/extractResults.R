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
extractResults <- function(x, trait, ava, type = "unavailableSpecies"){
  RES <- list(call = match.call())
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  typeMETHOD <- c("unavailableSpecies")
  type <- pmatch(type, typeMETHOD)
  if (length(type) > 1) {
    stop("Only one argument is accepted in type")
  }
  if (is.na(type)) {
    stop("Invalid type")
  }
  if (type == 1) { # unavailableSpecies
    if(inherits(x, "simRest")){
      comp <- x$simulation$composition
    } else{
      comp <- x$selection$composition
    }
    sppSelected <- comp[, colSums(comp)>0]
    sppSelected <- colnames(sppSelected)
    traitsNames <- colnames(trait)
    if(!inherits(ava, 'character') || !all(ava %in% traitsNames) || length(ava)>1){
      stop("ava must be a character indicating a single column of the trait data frame")
    }
    unaSpp <- rownames(trait)[!as.logical(trait[,ava])]
    RES$necessarySpp <- data.frame(UnavailableSpp = intersect(unaSpp, sppSelected))  
  }
  return(RES)
}
#' @title extractResults
#' @description Check the unavailable species that were present in selected communities these are the species necessary to achieve the thresholds when selected communities have at least one species not available on the market
#' @details
#' @encoding UTF-8
#' @aliases 
#' @param x A object of class "simRest" or "simRestSelect" to visualize results
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param type unavailableSpecies
#' @returns 
#' @note 
#' @author 
#' @seealso
#' @references
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
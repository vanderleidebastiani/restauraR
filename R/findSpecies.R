#' @title Internal function to select species to generates simulated communities
#' @encoding UTF-8
#' @importFrom Select selectSpecies
#' @importFrom utils capture.output
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param cwm A vector with trait names to constrain Community Weighted Mean (CWM) while maximising functional diversity. Constraints are driven over the range of each trait.
#' @param rao A vector with traits names to be considered in maximize functional diversity (Rao Quadratic Entropy), or distance matrix (class "dist").
#' @param n Number of species to select.
#' @param phi A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy.
#' @returns A vector with selected species names based on their traits and a desired trait profile.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{propMatrix}}
#' @keywords Auxiliary
#' @export
findSpecies <- function(traits, cwm, rao, n, phi){
  nSpeciesInt <- nrow(traits)
  species <- rownames(traits)
  if(inherits(rao, 'character')){
    t2d <- as.matrix(scale(traits[, rao, drop = FALSE]))
    # } else if(inherits(rao, 'dist')){
    #   t2d <- rao
    # }  
  } else {
    t2d <- as.matrix(rao)
    # Organize distance matrix
    match.names <- match(species, colnames(t2d))
    t2d <- t2d[match.names, match.names, drop = FALSE]
  }
  if(!is.null(cwm)){
    t2c <- as.matrix(traits[ , cwm, drop = FALSE])
    constraints <- apply(t2c, 2, function(x){
      cons <- seq(min(x), max(x), length.out = 8)
      return(cons[c(-1,-8)])
    })
    selSpp <- vector(mode = "list", length = length(cwm))
    names(selSpp) <- cwm
    for(i in cwm){
      cons_i <- constraints[ , i]
      t2c_i <- t2c[ , i, drop = FALSE]
      selSpp_i <- matrix(NA, nrow = nSpeciesInt, ncol = 6)
      for(j in 1:length(cons_i)){
        jj <- cons_i[j]
        names(jj) <- colnames(t2c_i)
        invisible(utils::capture.output(selSpp_j <- Select::selectSpecies(t2c = t2c_i, constraints = jj, t2d = t2d, phi = phi, obj = "QH")))    
        selSpp_i[,j] <- selSpp_j$prob
      }
      selSpp[[i]] <- selSpp_i 
    }
    propMatrixSelSpp <- do.call(cbind, selSpp)
    sppMax <- c()
    propMin <- 0.1
    while(length(sppMax) < n){
      sppMax <- which(propMatrixSelSpp > propMin, arr.ind = TRUE)
      sppMax <- unique(species[sppMax[,1]])
      propMin <- 0.5*propMin
      if(length(sppMax) == nSpeciesInt){
        break
      }
    }
  } else {
    invisible(utils::capture.output(selSpp <- Select::selectSpecies(t2d = t2d, phi = phi, obj = "QH")))
    propMatrixSelSpp <- selSpp$prob
    sppMax <- c()
    propMin <- 0.1
    while(length(sppMax) < n){
      sppMax <- species[propMatrixSelSpp > propMin]
      propMin <- 0.5*propMin
    }
  }
  return(sppMax)
}
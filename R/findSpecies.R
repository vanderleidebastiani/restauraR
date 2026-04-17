#' @title Internal function to select species to generate simulated communities
#' @encoding UTF-8
#' @importFrom Select selectSpecies
#' @importFrom utils capture.output
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param maxDiver Character vector specifying trait names to functional diversity optimisation (Rao Quadratic Entropy), or distance matrix (object of class "dist").
#' @param constCWM Character vector specifying trait names to constrain Community Weighted Mean (CWM) while maximising functional diversity. Constraints are driven across the range of each trait.
#' @param n Number of species to select.
#' @param phi Numeric parameter bounded between 0 and 1 that weights the relative importance of either quadratic entropy or entropy.
#' @returns A vector with names of selected species, chosen based on their traits and desired trait profile.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{generateCommunityMatrices}}
#' @keywords InternalFunction
findSpecies <- function(traits, maxDiver, constCWM, n, phi){
  nSpeciesInt <- nrow(traits)
  species <- rownames(traits)
  if(inherits(maxDiver, "character")){
    t2d <- as.matrix(scale(traits[, maxDiver, drop = FALSE]))
  } else {
    t2d <- as.matrix(maxDiver)
    # Organize distance matrix
    match.names <- match(species, colnames(t2d))
    t2d <- t2d[match.names, match.names, drop = FALSE]
    t2d <- stats::as.dist(t2d)
  }
  if(!is.null(constCWM)){
    t2c <- as.matrix(traits[ , constCWM, drop = FALSE])
    constraints <- apply(t2c, 2, function(x){
      cons <- seq(min(x), max(x), length.out = 8)
      return(cons[c(-1,-8)])
    })
    selSpp <- vector(mode = "list", length = length(constCWM))
    names(selSpp) <- constCWM
    for(i in constCWM){
      cons_i <- constraints[ , i]
      t2c_i <- t2c[ , i, drop = FALSE]
      selSpp_i <- matrix(NA, nrow = nSpeciesInt, ncol = 6)
      for(j in 1:length(cons_i)){
        jj <- cons_i[j]
        names(jj) <- colnames(t2c_i)
        invisible(utils::capture.output(selSpp_j <- Select::selectSpecies(t2c = t2c_i, constraints = jj, t2d = t2d, phi = phi, obj = "QH", euclid = FALSE)))    
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
    invisible(utils::capture.output(selSpp <- Select::selectSpecies(t2d = t2d, phi = phi, obj = "QH", euclid = FALSE)))
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
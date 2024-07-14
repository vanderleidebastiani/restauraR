#' @title findSpecies
#' @description findSpecies
#' @details
#' @encoding UTF-8
#' @importFrom Select selectSpecies
#' @aliases
#' @param trait
#' @param rao
#' @param cwm
#' @param n
#' @param phi
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
findSpecies <- function(trait, rao, cwm, n, phi){
  nSpeciesInt <- nrow(trait)
  species <- rownames(trait)
  if(inherits(rao, 'character')){
    t2d <- as.matrix(scale(trait[, rao, drop = FALSE]))
  } else if(inherits(rao, 'dist')){
    t2d <- rao
  }
  if(!missing(cwm)){
    t2c <- as.matrix(trait[ , cwm, drop = FALSE])
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
        # CONFERIR obj ----
        invisible(capture.output(selSpp_j <- Select::selectSpecies(t2c = t2c_i, constraints = jj, t2d = t2d, phi = phi, obj = "QH")))
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
    }
  } else {
    # CONFERIR obj ----
    invisible(capture.output(selSpp <- Select::selectSpecies(t2d = t2d, phi = phi, obj = "QH")))
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
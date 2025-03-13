#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
adjustSimulations <- function(x, minAbun = NULL, reallocate = FALSE) {
  # Check object class
  if(!c(inherits(x, "simRest"))){
    stop("x must be of the simRest class")
  }
  # Extract composition
  comp <- x$simulation$composition
  # Check if all number are integer
  allInteger <- all(comp%%1==0)
  # Extract baseline
  baseline <- x$simulation$baseline
  # Calculate additions
  compAdditions <- comp - baseline
  compAdditions
  # If proportions
  if(!allInteger){
    compAdditions <- (comp*2) - baseline
    compAdditions
  }
  if(!is.null(minAbun)){
    # Adjust each simulation
    for(i in 1:nrow(compAdditions)){
      # Test
      testTemp <- compAdditions[i, ] > 0 & compAdditions[i,] <= minAbun
      remPart <- sum(compAdditions[i, testTemp])
      # Set zero to rare species
      compAdditions[i, testTemp] <- 0
      # Reallocate individuals
      if(reallocate && allInteger){
        # Species with non zero abundances
        sppWithAbundance <- names(which(compAdditions[i,] > 0))
        if(remPart > 0 && length(sppWithAbundance) > 0 ){
          for(k in 1:remPart){
            pos <- sample(sppWithAbundance, 1)
            compAdditions[i, pos] <- compAdditions[i, pos]+1
          }
        }
      }
    }
    # Recalculate composition
    comp <- baseline + compAdditions
  }
  # If proportions
  if(!allInteger){
    # (Re)calculate species proportions
    comp <- sweep(comp, MARGIN = 1, rowSums(comp), FUN = "/")
  }
  # Composicao pode ter linhas e/ou colunas com tudo zero. Remover?
  # se sim ver ver grupos 
  rowRem <- rowSums(comp, na.rm = TRUE)==0
  # comp <- comp[!rowRem,]
  # Put back composition
  x$simulation$composition <- comp[!rowRem,]
  # Remove information from simulation$group
  x$simulation$group <- x$simulation$group[!rowRem,]
  # Remove information from simulation$baseline
  x$simulation$baseline <- x$simulation$baseline[!rowRem,]
  return(x)
}
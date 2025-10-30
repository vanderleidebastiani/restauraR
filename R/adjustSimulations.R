#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
adjustSimulations <- function (x, minAbun = NULL, inv = NULL, reallocate = FALSE) {
  # Check object class
  if (!c(inherits(x, "simRest"))) {
    stop("The x argument must be of class simRest")
  }
  # Extract composition
  comp <- x$simulation$composition
  # Check if all number are integer
  allInteger <- all(comp%%1 == 0)
  # Extract baseline
  baseline <- x$simulation$baseline
  # Calculate additions
  # If proportions
  if(!allInteger){
    compAdditions <- (comp*2) - baseline
  } else{ # If counts
    compAdditions <- comp - baseline
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
          # CONFERIR ESSA MUDANCA
          # for(k in 1:remPart){
          #   pos <- sample(sppWithAbundance, 1)
          #   compAdditions[i, pos] <- compAdditions[i, pos]+1
          # }
          
          # # O metodo abaixo exige menos tempo de processamento:
          # nspp <- sample(1:length(sppWithAbundance), 1) #number of sim spp
          # if(nspp>remPart){nspp <- remPart} #nspp cant be higher than rempart
          # newAbun <- generateRandomIntegers(remPart, nspp) #generate one random integer for each nspp, with sum equal to remPart
          # pos <- sample(sppWithAbundance, nspp) #sample which species will receive new individuals
          # compAdditions[i,pos] <- compAdditions[i,pos] + newAbun #add new abundances
          
          # O metodo abaixo usa a probabilidade de sorteio da especie:
          nspp <- sample(1:length(sppWithAbundance), 1) #number of sim spp
          prob <- compAdditions[i,]/sum(compAdditions[i,]) #sampling probability
          newAbun <- rmultinom(1, size=remPart, prob=prob) #generate one integer for each nspp (including 0), based on prob, with sum equal to remPart
          pos <- sample(sppWithAbundance, nspp) #sample which species will receive new individuals
          compAdditions[i,pos] <- compAdditions[i,pos] + newAbun #add new abundances
        }
      }
    }
    # Recalculate composition
    comp <- baseline + compAdditions
  }
  if (!is.null(inv)) {
    for (i in 1:nrow(compAdditions)){
      testTemp <- removeCoex(comp[i,], compAdditions[i,], inv)
      remPart <- sum(compAdditions[i, testTemp])
      compAdditions[i, testTemp] <- 0
      if (reallocate && allInteger) {
        sppWithAbundance <- names(which(compAdditions[i, 
        ] > 0))
        if (remPart > 0 && length(sppWithAbundance) > 0) {
          n <- sample(1:length(sppWithAbundance), 1)
          if(n>remPart){n <- remPart}
          prob <- compAdditions[i,]/sum(compAdditions[i,])
          newAbun <- rmultinom(1, size=remPart, prob=prob)
          pos <- sample(sppWithAbundance, n)
          compAdditions[i,pos] <- compAdditions[i,pos] + newAbun
        }
      }
    }
    comp <- baseline + compAdditions
  }
  # If proportions
  if (!allInteger) {
    # (Re)calculate species proportions
    comp <- sweep(comp, MARGIN = 1, rowSums(comp), FUN = "/")
  }
  rowRem <- rowSums(comp, na.rm = TRUE) == 0
  # Put back composition
  x$simulation$composition <- comp[!rowRem,]
  # Remove information from simulation$group
  x$simulation$group <- x$simulation$group[!rowRem,]
  # Remove information from simulation$baseline
  x$simulation$baseline <- x$simulation$baseline[!rowRem,]
  return(x)
}
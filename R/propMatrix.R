#' @title propMatrix
#' @description propMatrix
#' @details
#' @encoding UTF-8
#' @aliases
#' @param trait
#' @param ava 
#' @param und 
#' @param it
#' @param rich
#' @param cwm
#' @param rao
#' @param phi
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
propMatrix <- function(trait, ava, und, it, rich, cwm, rao, phi){
  # Auxiliary functions
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  sampleAbundance <- function(nRich1, nRich2, vLen){
    nsp_i <-  resample(nRich1:nRich2, 1)
    ocor <- sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
    abund <- rlnorm(vLen)
    abund <- abund * ocor
    prop <- abund/sum(abund)
    return(prop)
  }
  # Print warning if ava is missing
  if(missing(ava)){
    warning("Missing the ava argument. All species are considered available")
  }
  # Remove undesired species
  if(!missing(und)){
    undLog <- as.logical(trait[,und])
    trait <- trait[!undLog,] # remove undesired species
  }
  # Basic input parameters
  nSpecies <- nrow(trait)
  species <- rownames(trait)
  # Check
  if(rich[1] > nSpecies){
    stop("Minimum richness is higher than number of species")
  }
  if(!missing(ava)){
    nAva <- sum(as.logical(trait[,ava]))
    if(rich[1] > nAva){
      stop("Minimum richness is higher than number of available species")
    }
  }
  # Number of iterations for simulations
  if(!missing(rao)){
    if(!missing(ava)){
      itMax <- round(0.25*it)
      itMaxAva <- round(0.25*it)
      itAva <- round(0.25*it)
      itAll <- it - itMax - itMaxAva - itAva
    } else{
      itMax <- round(0.5*it)
      itAll <- it - itMax
    }
  } else {
    if(!missing(ava)){
      itAva <- round(0.5*it)
      itAll <- it - itAva
    } else{
      itAll <- it
    }
  }
  # Run simulation with all available species
  if(!missing(ava)){
    avaLog <- as.logical(trait[,ava])
    if(sum(avaLog) < rich[2]){
      nsp <- sum(avaLog) 
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- sum(avaLog)
    }
    propMatrixAva <- matrix(0, ncol = nSpecies, nrow = itAva)
    for(i in 1:itAva){
      propMatrixAva[i, avaLog] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
    }
  }
  # Run simulation with all species
  propMatrixPool <- matrix(0, ncol = nSpecies, nrow = itAll)
  for(i in 1:itAll){
    propMatrixPool[i,] <- sampleAbundance(nRich1 = rich[1], nRich2 = rich[2], vLen = nSpecies)
  }
  # Maximize diversity
  if(!missing(rao)){
    # Find distant species 
    sppMax <- findSpecies(trait, rao, cwm, rich[1], phi)
    # Run simulation with species that maximize rao
    if(length(sppMax) < rich[2]){
      nsp <- length(sppMax)
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- length(sppMax)
    }
    propMatrixSelSpp2 <- matrix(0, ncol = nSpecies, nrow = itMax)
    sppMaxPos <- species %in% sppMax
    for(i in 1:itMax){
      propMatrixSelSpp2[i, sppMaxPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
    }
    if(!missing(ava)){
      # Find distant species that are available
      avaLog <- as.logical(trait[,ava])
      sppMaxAva <- findSpecies(trait[avaLog, ], rao, cwm, rich[1], phi)
      # Run simulation with species that maximize rao and are available
      if(length(sppMaxAva) < rich[2]){
        nsp <- length(sppMaxAva) 
        vLen <- nsp
      } else {
        nsp <- rich[2]
        vLen <- length(sppMaxAva)
      }
      propMatrixSelSppAva2 <- matrix(0, ncol = nSpecies, nrow = itMaxAva)
      sppMaxAvaPos <- species %in% sppMaxAva
      for(i in 1:itMaxAva){
        propMatrixSelSppAva2[i, sppMaxAvaPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
      }
    }
  }
  # Bind all matrices
  if(!missing(rao)){
    if(!missing(ava)){
      propMatrix <- rbind(propMatrixSelSpp2, propMatrixSelSppAva2,
                          propMatrixAva, propMatrixPool)
    } else{
      propMatrix <- rbind(propMatrixSelSpp2,
                          propMatrixPool)
    }
  } else {
    if(!missing(ava)){
      propMatrix <- rbind(propMatrixAva, propMatrixPool)
    } else{
      propMatrix <- propMatrixPool
    }
  }
  rownames(propMatrix) <- sprintf("sim%d",seq_len(nrow(propMatrix)))
  colnames(propMatrix) <- species
  return(propMatrix)
}
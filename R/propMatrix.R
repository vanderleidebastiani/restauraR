#' @title Internal function to generate communities proportion matrix
#' @encoding UTF-8
#' @importFrom stats rlnorm
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param und A vector indicating trait name which indicates undesired species (1 or 0) in trait data.
#' @param it Number of iterations (communities).
#' @param rich The range of richness values in each community.
#' @param cwm A vector with traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist).
#' @param phi A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy.
#' @param nInd The number of individuals to draw. Used only in method "individuals".
#' @param cvAbund Coefficient of variation (cv) of the relative abundances in the species pool. Used only in method "individuals".
#' @param prob A vector indicating trait name which indicates the probabilities to draw individuals in each species. Used only in method "individuals".
#' @param method Method to obtain the samples, "proportions" or "individuals" (Default method = "proportions").
#' @param group A vector with traits name which indicates the group to which species belongs.
#' @param probGroupRich Vector of probabilities to draw species richness in each group.
#' @param probGroupAbund Vector of probabilities to draw individuals or relative abundances in each group.
#' @returns A community matrix with species relative abundances.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{findSpecies}}
#' @keywords Auxiliary
#' @export
propMatrix <- function(trait, ava, und, it, rich, cwm, rao, phi, nInd, cvAbund, prob, method, group, probGroupRich, probGroupAbund){
  # nInd = nInd, cvAbund = cvAbund, prob = prob, method = method
  # # Auxiliary functions
  # resample <- function(x, ...) x[sample.int(length(x), ...)]
  # sampleAbundance <- function(nRich1, nRich2, vLen){
  #   nsp_i <-  resample(nRich1:nRich2, 1)
  #   ocor <- sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
  #   abund <- stats::rlnorm(vLen)
  #   abund <- abund * ocor
  #   prop <- abund/sum(abund)
  #   return(prop)
  # }
  # Print warning if ava is missing
  # if(is.null(ava)){
  #   warning("Missing the ava argument. All species are considered available")
  # }
  # Remove undesired species
  if(!is.null(und)){
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
  if(!is.null(ava)){
    nAva <- sum(as.logical(trait[,ava]))
    if(rich[1] > nAva){
      stop("Minimum richness is higher than number of available species")
    }
  }
  # Set number of iterations for simulations
  if(!is.null(rao)){
    if(!is.null(ava)){
      itMax <- round(0.25*it)
      itMaxAva <- round(0.25*it)
      itAva <- round(0.25*it)
      itAll <- it - itMax - itMaxAva - itAva
    } else{
      itMax <- round(0.5*it)
      itAll <- it - itMax
    }
  } else {
    if(!is.null(ava)){
      itAva <- round(0.5*it)
      itAll <- it - itAva
    } else{
      itAll <- it
    }
  }
  # Probabilities to draw individuals
  if(!is.null(prob)){
    probVector <- trait[,prob]
  } else{
    probVector <- NULL
  }
  # Simulation in each group of species
  if(!is.null(group)){
    group <- as.character(trait[, group])
    uniqueGroups <- unique(group)
    # probGroupRich is optional
    if(!is.null(probGroupRich)){
      if(!all(names(probGroupRich) %in% uniqueGroups)){
        stop("names in probGroupRich must match the group names")
      }  
    }
    # probGroupAbund is mandatory with group argument
    if(!all(names(probGroupAbund) %in% uniqueGroups)){
      stop("names in probGroupAbund must match the group names")
    } 
  }
  # Run simulation with all available species
  if(!is.null(ava)){
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
      # propMatrixAva[i, avaLog] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, sPool = vLen)
      if(!is.null(group)){
        propMatrixAva[i, avaLog] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                          nRich2 = nsp, 
                                                          nInd = nInd, 
                                                          cvAbund = cvAbund,
                                                          prob = probVector[avaLog], 
                                                          method = method,
                                                          group = group[avaLog], 
                                                          probGroupRich = probGroupRich, 
                                                          probGroupAbund = probGroupAbund)
      } else{
        propMatrixAva[i, avaLog] <- sampleAbundance(nRich1 = rich[1], 
                                                    nRich2 = nsp, 
                                                    sPool = vLen, 
                                                    nInd = nInd, 
                                                    cvAbund = cvAbund, 
                                                    prob = probVector[avaLog], 
                                                    method = method)  
      }
    }
  }
  # Run simulation with all species
  propMatrixPool <- matrix(0, ncol = nSpecies, nrow = itAll)
  for(i in 1:itAll){
    # propMatrixPool[i,] <- sampleAbundance(nRich1 = rich[1], nRich2 = rich[2], sPool = nSpecies)
    if(!is.null(group)){
      propMatrixPool[i, ] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                   nRich2 = rich[2], 
                                                   nInd = nInd, 
                                                   cvAbund = cvAbund,
                                                   prob = probVector, 
                                                   method = method,
                                                   group = group, 
                                                   probGroupRich = probGroupRich, 
                                                   probGroupAbund = probGroupAbund)
    } else{
      propMatrixPool[i,] <- sampleAbundance(nRich1 = rich[1], 
                                            nRich2 = rich[2], 
                                            sPool = nSpecies, 
                                            nInd = nInd, 
                                            cvAbund = cvAbund, 
                                            prob = probVector, 
                                            method = method)
    }
  }
  # Maximize diversity
  if(!is.null(rao)){
    # Find distant species 
    sppMax <- findSpecies(trait, cwm, rao, rich[1], phi)
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
      # propMatrixSelSpp2[i, sppMaxPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, sPool = vLen)
      if(!is.null(group)){
        propMatrixSelSpp2[i, sppMaxPos] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                                 nRich2 = nsp, 
                                                                 nInd = nInd, 
                                                                 cvAbund = cvAbund,
                                                                 prob = probVector[sppMaxPos], 
                                                                 method = method,
                                                                 group = group[sppMaxPos], 
                                                                 probGroupRich = probGroupRich, 
                                                                 probGroupAbund = probGroupAbund)
      } else{
        propMatrixSelSpp2[i, sppMaxPos] <- sampleAbundance(nRich1 = rich[1], 
                                                           nRich2 = nsp, 
                                                           sPool = vLen, 
                                                           nInd = nInd, 
                                                           cvAbund = cvAbund, 
                                                           prob = probVector[sppMaxPos], 
                                                           method = method)
      }
    }
    if(!is.null(ava)){
      # Find distant species that are available
      avaLog <- as.logical(trait[,ava])
      sppMaxAva <- findSpecies(trait[avaLog, ], cwm, rao, rich[1], phi)
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
        # propMatrixSelSppAva2[i, sppMaxAvaPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, sPool = vLen)
        if(!is.null(group)){
          propMatrixSelSppAva2[i, sppMaxAvaPos] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                                         nRich2 = nsp, 
                                                                         nInd = nInd, 
                                                                         cvAbund = cvAbund,
                                                                         prob = probVector[sppMaxAvaPos], 
                                                                         method = method,
                                                                         group = group[sppMaxAvaPos], 
                                                                         probGroupRich = probGroupRich, 
                                                                         probGroupAbund = probGroupAbund)
        } else{
          propMatrixSelSppAva2[i, sppMaxAvaPos] <- sampleAbundance(nRich1 = rich[1], 
                                                                   nRich2 = nsp, 
                                                                   sPool = vLen, 
                                                                   nInd = nInd, 
                                                                   cvAbund = cvAbund, 
                                                                   prob = probVector[sppMaxAvaPos], 
                                                                   method = method)
        }
      }
    }
  }
  # Bind all matrices
  if(!is.null(rao)){
    if(!is.null(ava)){
      propMatrix <- rbind(propMatrixSelSpp2, propMatrixSelSppAva2,
                          propMatrixAva, propMatrixPool)
    } else{
      propMatrix <- rbind(propMatrixSelSpp2,
                          propMatrixPool)
    }
  } else {
    if(!is.null(ava)){
      propMatrix <- rbind(propMatrixAva, propMatrixPool)
    } else{
      propMatrix <- propMatrixPool
    }
  }
  rownames(propMatrix) <- sprintf("sim%d", seq_len(nrow(propMatrix)))
  colnames(propMatrix) <- species
  return(propMatrix)
}
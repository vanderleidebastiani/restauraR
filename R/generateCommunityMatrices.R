#' @title Internal function to generate community matrices
#' @description Create simulated community matrices through constrained species assembly from a regional pool. Supports multiple ecological constraints, including functional diversity optimisation, community-weighted mean constraints, co-occurrence patterns, and group-based sampling.
#' @encoding UTF-8
#' @importFrom stats rlnorm
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava Character vector specifying the trait name that indicates the availability of species in traits data (binary: 1 = available, 0 = unavailable).
#' @param und Character vector specifying the trait name that indicates undesired species in traits data (binary: 1 = undesired, 0 = desired).
#' @param it Number of iterations (communities) to generate.
#' @param rich Numeric vector of length 2 specifying the richness range in each community.
#' @param maxDiver Character vector specifying trait names to functional diversity optimisation (Rao Quadratic Entropy), or distance matrix (object of class "dist").
#' @param constCWM Character vector specifying trait names to constrain Community Weighted Mean (CWM) while maximising functional diversity. Constraints are driven across the range of each trait.
#' @param phi Numeric parameter bounded between 0 and 1 that weights the relative importance of either quadratic entropy or entropy.
#' @param nInd Numeric vector of length 2 specifying the number of individuals to draw. Used only in the method "individuals".
#' @param cvAbund Coefficient of variation (cv) of the relative abundances in the species pool. Used only in the method "individuals".
#' @param prob Character vector specifying the trait name that indicates the probabilities to draw individuals in each species. Used only in the method "individuals".
#' @param method Method to obtain the samples, "proportions" or "individuals" (Default method = "proportions").
#' @param cooccur Matrix with co-occurrence probabilities between species to constrain ecological assembly patterns.
#' @param minAbund Minimal abundance or proportion threshold to maintain for each species in simulated communities. 
#' @param group Character vector specifying the trait name that indicates the group to which the species belongs.
#' @param probGroupRich Numeric vector of probabilities to draw species richness in each group.
#' @param probGroupAbund Numeric vector of probabilities to draw individuals or relative abundances in each group.
#' @returns A community matrix with sites as rows and species as columns, containing either relative species proportions or raw species abundances counts.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{findSpecies}}
#' @keywords Auxiliary
#' @export
generateCommunityMatrices <- function(traits, ava, und, it, rich, maxDiver, constCWM, phi, nInd, cvAbund, prob, method, cooccur, minAbund, group, probGroupRich, probGroupAbund){
  # Remove undesired species
  if(!is.null(und)){
    undLog <- as.logical(traits[,und])
    traits <- traits[!undLog,] # remove undesired species
    if(!is.null(cooccur)){
      cooccur <- cooccur[!undLog, !undLog] # remove undesired species
    }
  }
  # Basic input parameters
  nSpecies <- nrow(traits)
  species <- rownames(traits)
  # Check
  if(rich[1] > nSpecies){
    stop("Minimum richness value exceeds total number of species")
  }
  if(!is.null(ava)){
    nAva <- sum(as.logical(traits[,ava]))
    if(rich[1] > nAva){
      stop("Minimum richness value exceeds number of available species")
    }
  }
  # Set number of iterations for simulations
  if(!is.null(maxDiver)){
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
    probVector <- traits[,prob]
  } else{
    probVector <- NULL
  }
  # Simulation in each group of species
  if(!is.null(group)){
    group <- as.character(traits[, group])
    uniqueGroups <- unique(group)
    # probGroupRich is optional
    if(!is.null(probGroupRich)){
      if(!all(names(probGroupRich) %in% uniqueGroups)){
        stop("The names in probGroupRich must match the group names")
      }  
    }
    # probGroupAbund is mandatory with group argument
    if(!all(names(probGroupAbund) %in% uniqueGroups)){
      stop("The names in probGroupAbund must match the group names")
    } 
  }
  # Run simulation with all available species
  if(!is.null(ava)){
    avaLog <- as.logical(traits[,ava])
    if(sum(avaLog) < rich[2]){
      nsp <- sum(avaLog) 
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- sum(avaLog)
    }
    cooccurTemp <- cooccur[avaLog, avaLog, drop = FALSE]
    propMatrixAva <- matrix(0, ncol = nSpecies, nrow = itAva)
    for(i in 1:itAva){
      if(!is.null(group)){
        propMatrixAva[i, avaLog] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                          nRich2 = nsp, 
                                                          nInd1 = nInd[1], 
                                                          nInd2 = nInd[2], 
                                                          cvAbund = cvAbund,
                                                          prob = probVector[avaLog], 
                                                          method = method,
                                                          cooccur = cooccurTemp,
                                                          minAbund = minAbund,
                                                          group = group[avaLog], 
                                                          probGroupRich = probGroupRich, 
                                                          probGroupAbund = probGroupAbund)
      } else{
        propMatrixAva[i, avaLog] <- sampleAbundance(nRich1 = rich[1], 
                                                    nRich2 = nsp, 
                                                    sPool = vLen, 
                                                    nInd1 = nInd[1], 
                                                    nInd2 = nInd[2], 
                                                    cvAbund = cvAbund, 
                                                    prob = probVector[avaLog], 
                                                    method = method,
                                                    cooccur = cooccurTemp,
                                                    minAbund = minAbund)  
      }
    }
  }
  # Run simulation with all species
  propMatrixPool <- matrix(0, ncol = nSpecies, nrow = itAll)
  for(i in 1:itAll){
    if(!is.null(group)){
      propMatrixPool[i, ] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                   nRich2 = rich[2], 
                                                   nInd1 = nInd[1], 
                                                   nInd2 = nInd[2], 
                                                   cvAbund = cvAbund,
                                                   prob = probVector, 
                                                   method = method,
                                                   cooccur = cooccur,
                                                   minAbund = minAbund,
                                                   group = group, 
                                                   probGroupRich = probGroupRich, 
                                                   probGroupAbund = probGroupAbund)
    } else{
      propMatrixPool[i,] <- sampleAbundance(nRich1 = rich[1], 
                                            nRich2 = rich[2], 
                                            sPool = nSpecies, 
                                            nInd1 = nInd[1], 
                                            nInd2 = nInd[2], 
                                            cvAbund = cvAbund, 
                                            prob = probVector, 
                                            method = method,
                                            cooccur = cooccur,
                                            minAbund = minAbund)
    }
  }
  # Maximize diversity
  if(!is.null(maxDiver)){
    # Find distant species 
    sppMax <- findSpecies(traits, maxDiver, constCWM, rich[1], phi)
    # Run simulation with species that maximize rao
    if(length(sppMax) < rich[2]){
      nsp <- length(sppMax)
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- length(sppMax)
    }
    propMatrixSelSpp <- matrix(0, ncol = nSpecies, nrow = itMax)
    sppMaxPos <- species %in% sppMax
    cooccurTemp <- cooccur[sppMaxPos, sppMaxPos, drop = FALSE]
    for(i in 1:itMax){
      if(!is.null(group)){
        propMatrixSelSpp[i, sppMaxPos] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                                nRich2 = nsp, 
                                                                nInd1 = nInd[1], 
                                                                nInd2 = nInd[2], 
                                                                cvAbund = cvAbund,
                                                                prob = probVector[sppMaxPos], 
                                                                method = method,
                                                                cooccur = cooccurTemp,
                                                                minAbund = minAbund,
                                                                group = group[sppMaxPos], 
                                                                probGroupRich = probGroupRich, 
                                                                probGroupAbund = probGroupAbund)
      } else{
        propMatrixSelSpp[i, sppMaxPos] <- sampleAbundance(nRich1 = rich[1], 
                                                          nRich2 = nsp, 
                                                          sPool = vLen, 
                                                          nInd1 = nInd[1], 
                                                          nInd2 = nInd[2], 
                                                          cvAbund = cvAbund, 
                                                          prob = probVector[sppMaxPos], 
                                                          method = method,
                                                          cooccur = cooccurTemp,
                                                          minAbund = minAbund)
      }
    }
    if(!is.null(ava)){
      # Find distant species that are available
      avaLog <- as.logical(traits[,ava])
      sppMaxAva <- findSpecies(traits[avaLog, ], maxDiver, constCWM, rich[1], phi)
      # Run simulation with species that maximize rao and are available
      if(length(sppMaxAva) < rich[2]){
        nsp <- length(sppMaxAva) 
        vLen <- nsp
      } else {
        nsp <- rich[2]
        vLen <- length(sppMaxAva)
      }
      propMatrixSelSppAva <- matrix(0, ncol = nSpecies, nrow = itMaxAva)
      sppMaxAvaPos <- species %in% sppMaxAva
      cooccurTemp <- cooccur[sppMaxAvaPos, sppMaxAvaPos, drop = FALSE]
      for(i in 1:itMaxAva){
        if(!is.null(group)){
          propMatrixSelSppAva[i, sppMaxAvaPos] <- sampleAbundanceGroups(nRich1 = rich[1],
                                                                        nRich2 = nsp, 
                                                                        nInd1 = nInd[1], 
                                                                        nInd2 = nInd[2], 
                                                                        cvAbund = cvAbund,
                                                                        prob = probVector[sppMaxAvaPos], 
                                                                        method = method,
                                                                        cooccur = cooccurTemp,
                                                                        minAbund = minAbund,
                                                                        group = group[sppMaxAvaPos], 
                                                                        probGroupRich = probGroupRich, 
                                                                        probGroupAbund = probGroupAbund)
        } else{
          propMatrixSelSppAva[i, sppMaxAvaPos] <- sampleAbundance(nRich1 = rich[1], 
                                                                  nRich2 = nsp, 
                                                                  sPool = vLen, 
                                                                  nInd1 = nInd[1], 
                                                                  nInd2 = nInd[2], 
                                                                  cvAbund = cvAbund, 
                                                                  prob = probVector[sppMaxAvaPos], 
                                                                  method = method,
                                                                  cooccur = cooccurTemp,
                                                                  minAbund = minAbund)
        }
      }
    }
  }
  # Bind all matrices
  if(!is.null(maxDiver)){
    if(!is.null(ava)){
      propMatrix <- rbind(propMatrixSelSpp, propMatrixSelSppAva,
                          propMatrixAva, propMatrixPool)
    } else{
      propMatrix <- rbind(propMatrixSelSpp,
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
  # rownames(propMatrix) <- seq_len(nrow(propMatrix))
  colnames(propMatrix) <- species
  # propMatrix
  return(propMatrix)
}
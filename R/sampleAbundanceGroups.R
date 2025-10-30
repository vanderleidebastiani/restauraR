#' @rdname sampleAbundance
#' @include sampleAbundance.R
#' @encoding UTF-8
#' @export
sampleAbundanceGroups <- function(nRich1, nRich2, nInd1, nInd2, cvAbund = 1, prob = NULL, returnProp = FALSE, method = "proportions", cooccur = NULL, group, probGroupRich, probGroupAbund){
  METHOD <- c("proportions", "individuals")
  method0 <- pmatch(method, METHOD)
  # The length of group is equal the sPool
  res <- rep(0, length = length(group))
  uniqueGroups <- unique(group)
  richTemp <- nRich1:nRich2
  # Sample richness
  nSppi <-  richTemp[sample.int(length(richTemp), 1)]
  if(method == 2 && !is.null(nInd1) && !is.null(nInd2)){ # Method individuals
    # Individuals vector
    nInd <- nInd1:nInd2
    # Sample total Individuals
    nInd <-  nInd[sample.int(length(nInd), 1)]  
  } else{
    nInd <- NULL
  }
  # Split richness in the groups
  if(!is.null(probGroupRich)){
    splitRichRand <- roundDivision(nSppi, probGroupRich[uniqueGroups])  
  } else{
    splitRichRand <- randDivision(nSppi, length(uniqueGroups))
  }
  # Standardize group probabilities
  probGroupAbund <- probGroupAbund/sum(probGroupAbund)
  # Split abundances in groups
  if(!is.null(nInd)){
    nIndTEMP <- roundDivision(nInd, probGroupAbund[uniqueGroups])
  } else{
    nIndTEMP <- NULL
  }
  # For each group
  for(l in 1:length(uniqueGroups)){
    # Filter
    secFilter <- group==uniqueGroups[l]
    # Number of species in current group
    nSppiTEMP <- sum(secFilter)
    if(nSppiTEMP<splitRichRand[l]){
      splitRichRand[l] <- nSppiTEMP
    }
    # Sample only if all key parameters are non-zero
    if(nSppiTEMP>0 && splitRichRand[l]>0 && (method0 == 1 || (method0 == 2 && nIndTEMP[l] > 0))){
      res[secFilter] <- sampleAbundance(nRich1 = splitRichRand[l], 
                                        nRich2 = splitRichRand[l], 
                                        sPool = nSppiTEMP, 
                                        nInd1 = nIndTEMP[l],
                                        nInd2 = nIndTEMP[l],
                                        cvAbund = cvAbund, 
                                        prob = prob[secFilter], 
                                        returnProp = returnProp,
                                        method = method,
                                        cooccur = cooccur)
      # If proportions recalculate the proportions
      if(method0 == 1){
        res[secFilter] <- res[secFilter]*probGroupAbund[uniqueGroups[l]]
      }
    }
  }
  # If method is proportions, recalculate the proportions after all groups defined
  if(method0 == 1){
    res <- res/sum(res)
  }
  return(res)
}
#' @rdname sampleAbundance
#' @include sampleAbundance.R
#' @encoding UTF-8
#' @export
sampleAbundanceGroups <- function(nRich1, nRich2, nInd, cvAbund = 1, prob = NULL, 
                                  returnProp = FALSE, method = "proportions",
                                  group, probGroupRich, probGroupAbund){
  METHOD <- c("proportions", "individuals")
  method0 <- pmatch(method, METHOD)
  # The length of group is equal the sPool
  res <- rep(0, length = length(group))
  uniqueGroups <- unique(group)
  richTemp <- nRich1:nRich2
  # Sample richness
  nSppi <-  richTemp[sample.int(length(richTemp), 1)]
  # Split richness in the groups
  if(!is.null(probGroupRich)){
    splitRichRand <- roundDivision(nSppi, probGroupRich[uniqueGroups])  
  } else{
    splitRichRand <- randDivision(nSppi, length(uniqueGroups))
  }
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
    # Combine filters
    # secFilter <- sppMaxAvaPos & secFilter
    # Number of species in current group
    nSppiTEMP <- sum(secFilter)
    if(nSppiTEMP<splitRichRand[l]){
      splitRichRand[l] <- nSppiTEMP
    }
    if(nSppiTEMP>0 && splitRichRand[l]>0 && nIndTEMP[l]){
      res[secFilter] <- sampleAbundance(nRich1 = splitRichRand[l], 
                                        nRich2 = splitRichRand[l], 
                                        sPool = nSppiTEMP, 
                                        nInd = nIndTEMP[l], 
                                        cvAbund = cvAbund, 
                                        prob = prob[secFilter], 
                                        returnProp = returnProp,
                                        method = method)
      # If proportions recalculate the proportions
      if(method0 == 1){
        res[secFilter] <- res[secFilter]*probGroupAbund[uniqueGroups[l]]
      }
    }
  }
  # If proportions recalculate the proportions after all groups set
  if(method0 == 1){
    res <- res/sum(res)
  }
  return(res)
}
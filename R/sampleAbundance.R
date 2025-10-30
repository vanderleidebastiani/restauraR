#' @title Internal function to sample species abundance or proportions
#' @encoding UTF-8
#' @importFrom stats rlnorm rmultinom
#' @param nRich1 Minimum richness for sample.
#' @param nRich2 Maximum richness for sample.
#' @param sPool Species pool size.
#' @param nInd1 Minimum number of individuals to draw. Used only in method "individuals".
#' @param nInd2 Maximum number of individuals to draw. Used only in method "individuals".
#' @param cvAbund Coefficient of variation (cv) of the relative abundances in the species pool. Used only in method "individuals".
#' @param prob Vector of probabilities to draw individuals in each species. Used only in method "individuals".
#' @param returnProp Logical argument to specify whether to return proportions of individuals rather than raw abundances.
#' @param method Method to obtain the samples, "proportions" or "individuals" (Default method = "proportions").
#' @param group A vector with the group to which each species belongs.
#' @param probGroupRich Vector of probabilities to draw species richness in each group.
#' @param probGroupAbund Vector of probabilities to draw individuals or relative abundances in each group.
#' @returns A vector with species abundance or proportions.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{propMatrix}}
#' @keywords Auxiliary
#' @export
sampleAbundance <- function(nRich1, nRich2, sPool, nInd1, nInd2, cvAbund = 1, prob = NULL, returnProp = FALSE, method = "proportions", cooccur = NULL){
  METHOD <- c("proportions", "individuals")
  method <- pmatch(method, METHOD)
	# Richness vector
	richness <- nRich1:nRich2
	# Sample richness
	nSppi <-  richness[sample.int(length(richness), 1)]
	if(method == 2 && !is.null(nInd1) && !is.null(nInd2)){ # Method individuals
	  # Individuals vector
	  nInd <- nInd1:nInd2
	  # Sample total Individuals
	  nInd <-  nInd[sample.int(length(nInd), 1)]  
	} else{
	  nInd <- NULL
	}
	# If cooccur is provided
	if(!is.null(cooccur)){
	  # Sample a reference species
	  refSpp <- sample.int(sPool, 1)
	  refSpp
	  # Co-occurrence probabilities
	  probOcorr <- cooccur[refSpp,]
	  probOcorr
	  # Species occurrence
	  ocor <- rep(0, sPool)
	  # Put reference species in occurrence vector
	  ocor[refSpp] <- 1
	  ocor
	  # If have enough species to sample
	  if(sum(probOcorr>0)>=(nSppi-1)){
	    sampledOcor <- sample(seq.int(sPool), nSppi-1, prob = probOcorr)
	    sampledOcor
	  } else{
	    # If all null (zero)
	    if(sum(probOcorr>0) == 0){
	      sampledOcor <- NULL
	    } else{
	      sampledOcor <- sample(seq.int(sPool), sum(probOcorr>0), prob = probOcorr)
	      sampledOcor
	    }
	  }
	  # Put sampled species in occurrence vector
	  ocor[sampledOcor] <- 1
	  ocor
	} else{
	  # Sample species occurrence 
	  ocor <- sample( c(rep(1, nSppi), rep(0, sPool - nSppi)))  
	}
	# Abundance vector
	abund <- ocor
	# Recalculate nSppi
	nSppi <- sum(ocor)
	if(method == 1){ # Method proportions
		abund0 <- stats::rlnorm(nSppi)
		abund0 <- abund0/sum(abund0)
	} 
	if(method == 2){ # Method individuals
		if(is.null(prob)){
		  # Calculate log normal coefficient
		  meanAbund <- nInd/nSppi
		  sdAbund <- meanAbund * cvAbund
		  sigma <- sqrt(log(sdAbund^2/meanAbund^2 + 1))
		  mu <- log(meanAbund) - sigma^2/2
		  exp(mu + sigma^2/2) # mean
		  sqrt(exp(2*mu+sigma^2)*(exp(sigma^2)-1)) # sd
		  # Random log normal distribution
		  pLogNor <- stats::rlnorm(nSppi, meanlog = mu, sdlog = sigma)
		} else{
		  # Use input probabilities
		  pLogNor <- prob[as.logical(ocor)]  
		}
		# Sampled abundance
		abund0 <- stats::rmultinom(1, nInd, pLogNor)[,1]
	}
	# Distribute abundance in species occurrence
	abund[ocor==1] <- abund0
	# Transform to proportions
	if(returnProp){
		abund <- abund/sum(abund)	
	}
	return(abund)
}
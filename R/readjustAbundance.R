#' @title Internal function to randomly adjust vectors of species abundance or proportions
#' @encoding UTF-8
#' @importFrom stats rmultinom
#' @param abund0 Numeric vector of species abundance or proportions to be adjusted.
#' @param minAbund Minimum abundance or proportion threshold to maintain for each species in simulated communities.
#' @param method The method used to obtain the samples, "proportions" or "individuals".
#' @returns A vector of adjusted species abundance or proportions.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{generateCommunityMatrices}}
#' @keywords InternalFunction
readjustAbundance <- function(abund0, minAbund, method) {
  # Only if length of abund0 larger than 1
  if(length(abund0)>1){
    METHOD <- c("proportions", "individuals")
    method <- pmatch(method, METHOD)
    # If proportions
    if(method == 1){
      # Adjust the min if necessary
      if(minAbund>(1/length(abund0))){
        # Include warning here?
        minAbund <- 1/length(abund0)  
      }
    } else{ # If individuals
      # Adjust min to floor of mean if necessary
      meanAbund0 <- mean(abund0)
      if(minAbund>meanAbund0){
        # Include warning here?
        minAbund <- floor(meanAbund0)
      }  
    }
    # Calc the total of deficit
    deficit <- sum(ifelse(abund0<minAbund, minAbund-abund0, 0))
    if(deficit>0){
      # Index below or above the minimal
      iBelow <- which(abund0 < minAbund)
      iAbove <- which(abund0 > minAbund)
      # Set the minimal abundance
      abund0[iBelow] <- minAbund
      # Abundance of above minus the minimum
      prodAdjust <- abund0[iAbove]-minAbund
      sum(prodAdjust)
      # Proportion of abundance of above minus the minimum
      prodAdjust <- prodAdjust/sum(prodAdjust)
      if(method == 1){
        indRemove <- deficit*prodAdjust
      } else{
        # Sample
        indRemove <- stats::rmultinom(1, deficit, prodAdjust)[,1]
      }
      # Update the abund0 vector
      abund0[iAbove] <- abund0[iAbove]-indRemove
      if(any(abund0<minAbund) && method == 2){
        # Recursive function to fine tuning
        abund0 <- readjustAbundance(abund0, minAbund, "individuals")
      }
    }
  }
  return(abund0)
}
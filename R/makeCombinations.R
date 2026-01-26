#' @title Internal function to make groups combinations
#' @description Create element combinations with group-based constraints.
#' @encoding UTF-8
#' @param x A vector with elements from which to generate the combinations.
#' @param groups A vector with the groups which the element belongs.
#' @param minSubset Minimum number of elements to include in each combination.
#' @param maxSubset Maximum number of elements to include in each combination.
#' @param maxComb Maximum number of combinations to generate.
#' @returns A binary matrix with all elements combinations.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
makeCombinations <- function(x, groups = NULL, minSubset = 1, maxSubset = 1, maxComb = 1000) {
  # Count combinations of the specified size
  fCountComb <- function(x, minSubset, maxSubset) {
    n <- length(x)
    maxK <- min(maxSubset, n)
    minK <- max(minSubset, 1)
    ks <- c(minK:maxK)
    # k <- ifelse(minK == maxK, maxK, ks)
    # res <- choose(n = n, k = k)
    res <- sum(unlist(lapply(ks, function(x) choose(n = n, k = x))))
    return(res)
  }
  # Sample function with size adjust
  fSample <- function(x, size){
    if(length(x)<size){
      size <- length(x)
    }
    res <- sample(x, size = size)
    return(res)
  }
  # Generate all combinations of the specified size
  fMakeComb <- function(x, minSubset, maxSubset) {
    n <- length(x)
    maxK <- min(maxSubset, n)
    minK <- max(minSubset, 1)
    if(minK > maxK){
      minK <- maxK
    }
    comb <- lapply(minK:maxK, function(k) utils::combn(x = x, k, simplify = FALSE))
    comb <- unlist(comb, recursive = FALSE)
    return(comb)
  }
  # If groups is equal to NULL, set a single group for all elements
  if (is.null(groups)) {
    # Count combinations
    nComb <- fCountComb(x, minSubset = minSubset, maxSubset = maxSubset)
    if(nComb <= maxComb){
      # Make all combinations
      comb <- fMakeComb(x = x, minSubset = minSubset, maxSubset = maxSubset)  
    } else{
      # Sample combinations
      comb <- lapply(1:maxComb, function(i) fSample(x, size = sample(minSubset:maxSubset, 1)))  
    }
  } else {
    # else, split the groups 
    splitGroups <- split(x = x, f = groups)
    # Count combinations
    nComb <- prod(unlist(lapply(splitGroups, fCountComb, minSubset = minSubset, maxSubset = maxSubset)))
    if(nComb <= maxComb){
      # Make all combinations
      # Generate subsets for all groups
      subsetGroups <- lapply(splitGroups, fMakeComb, minSubset = minSubset, maxSubset = maxSubset)
      # Cross-matching between groups
      comb <- expand.grid(subsetGroups, stringsAsFactors = FALSE)
      # Unlist 
      comb <- lapply(1:nrow(comb), function(i) unlist(comb[i, ], use.names = FALSE))
    } else{
      # Sample combinations
      comb <- lapply(1:maxComb, function(i) unlist(sapply(splitGroups, fSample, size = sample(minSubset:maxSubset, 1))))
    }
  }
  # Binary matrix
  res <- t(sapply(comb, function(i) as.integer(x %in% i)))
  colnames(res) <- x
  rownames(res) <- paste0("Comb_", seq.int(nrow(res)))
  return(res)
}
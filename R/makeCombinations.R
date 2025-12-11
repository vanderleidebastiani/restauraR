#' @title Internal function to make groups combinations
#' @description Make groups combinations with groups restriction.
#' @encoding UTF-8
#' @param x A vector with elements to make the combinations.
#' @param groups A vector with the groups which the element belongs.
#' @param minSubset Minimum elements in each subset.
#' @param maxSubset Maximum elements in each subset.
#' @returns A binary matrix with all elements combinations.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords Auxiliary
#' @export
makeCombinations <- function(x, groups = NULL, minSubset = 1, maxSubset = 1) {
  # Generate combinations of the specified size
  fMakeComb <- function(x, minSubset, maxSubset) {
    n <- length(x)
    maxK <- min(maxSubset, n)
    minK <- max(minSubset, 1)
    comb <- lapply(minK:maxK, function(k) utils::combn(x = x, k, simplify = FALSE))
    comb <- unlist(comb, recursive = FALSE)
    return(comb)
  }
  # If groups is equal to NULL, set a single group for all elements
  if (is.null(groups)) {
    comb <- fMakeComb(x = x, minSubset = minSubset, maxSubset = maxSubset)
  } else {
    # else, split the groups 
    splitGroups <- split(x = x, f = groups)
    # Generate subsets for all groups
    subconjuntos_por_grupo <- lapply(splitGroups, fMakeComb, minSubset = minSubset, maxSubset = maxSubset)
    # Cross-matching between groups
    comb <- expand.grid(subconjuntos_por_grupo, stringsAsFactors = FALSE)
    # Unlist 
    comb <- lapply(1:nrow(comb), function(i) unlist(comb[i, ], use.names = FALSE))
  }
  # Binary matrix
  res <- t(sapply(comb, function(i) as.integer(x %in% i)))
  colnames(res) <- x
  rownames(res) <- paste0("Comb_", seq.int(nrow(res)))
  return(res)
}
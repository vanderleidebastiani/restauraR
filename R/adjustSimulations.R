#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
adjustSimulations <- function (x, minAbun = NULL, inv = NULL, reallocate = FALSE) 
{
  if (!c(inherits(x, "simRest"))) {
    stop("x must be of the simRest class")
  }
  comp <- x$simulation$composition
  allInteger <- all(comp%%1 == 0)
  baseline <- x$simulation$baseline
  compAdditions <- comp - baseline
  if (!allInteger) {
    compAdditions <- (comp * 2) - baseline
    compAdditions
  }
  if (!is.null(minAbun)) {
    for (i in 1:nrow(compAdditions)) {
      testTemp <- compAdditions[i, ] > 0 & compAdditions[i,] <= minAbun
      remPart <- sum(compAdditions[i, testTemp])
      compAdditions[i, testTemp] <- 0
      if (reallocate && allInteger) {
        sppWithAbundance <- names(which(compAdditions[i,] > 0))
        if (remPart > 0 && length(sppWithAbundance) > 0) {
          nspp <- sample(1:length(sppWithAbundance), 1)
          if(nspp>remPart){nspp <- remPart}
          newAbun <- generateRandomIntegers(remPart, nspp)
          pos <- sample(sppWithAbundance, nspp)
          compAdditions[i,pos] <- compAdditions[i,pos] + newAbun
        }
      }
    }
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
          newAbun <- generateRandomIntegers(remPart, n)
          pos <- sample(sppWithAbundance, n)
          compAdditions[i,pos] <- compAdditions[i,pos] + newAbun
        }
      }
    }
    comp <- baseline + compAdditions
  }
  
  if (!allInteger) {
    comp <- sweep(comp, MARGIN = 1, rowSums(comp), FUN = "/")
  }
  rowRem <- rowSums(comp, na.rm = TRUE) == 0
  x$simulation$composition <- comp[!rowRem, ]
  x$simulation$group <- x$simulation$group[!rowRem, ]
  x$simulation$baseline <- x$simulation$baseline[!rowRem, ]
  return(x)
}
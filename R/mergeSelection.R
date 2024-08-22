#' @rdname selectCommunities
#' @include selectCommunities.R
#' @encoding UTF-8
#' @export
mergeSelection <- function(...) {
  # RES <- vector("list")
  RES <- list(call = match.call())
  ARGS <- list(...)
  # Incluir checagem
  sapply(ARGS, function(x) inherits(x, "simRestSelect"))
  # Group
  group <- lapply(ARGS, function(x) x$selection$group)
  group <- lapply(group, as.data.table, keep.rownames = FALSE)
  group <- data.table::rbindlist(group, use.names = TRUE, fill = TRUE)
  RES$selection$group <- data.frame(group)
  # Composition
  comp <- lapply(ARGS, function(x) x$selection$composition)
  comp <- lapply(comp, as.data.table, keep.rownames = TRUE)
  comp <- data.table::rbindlist(comp, use.names = TRUE, fill = TRUE)
  compRowNames <- comp$rn
  comp <- as.matrix(comp[,-1])
  rownames(comp) <- compRowNames
  comp[is.na(comp)] <- 0
  RES$selection$composition <- comp
  # Results
  results <- lapply(ARGS, function(x) x$selection$results)
  results <- lapply(results, as.data.table, keep.rownames = TRUE)
  results <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  resultRowNames <- results$rn
  results <- as.data.frame(results[,-1])
  rownames(results) <- resultRowNames
  RES$selection$results <- results
  x <- ARGS[[1]] # Extract only reference and supplementary information for the first object
  if(!is.null(x$reference$composition)){
    RES$reference$composition <- x$reference$composition
  }
  if(!is.null(x$supplementary$composition)){
    RES$supplementary$composition <- x$supplementary$composition
  }
  if(!is.null(x$reference$results)){
    RES$reference$results <- x$reference$results
  }
  if(!is.null(x$supplementary$results)){
    RES$supplementary$results <- x$supplementary$results
  }
  class(RES) <- "simRestSelect"
  return(RES)
  # Precisa incluir a multifunctionality, thresholds e N?
}

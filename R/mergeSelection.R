#' @rdname selectCommunities
#' @include selectCommunities.R
#' @encoding UTF-8
#' @export
mergeSelection <- function(...) {
  RES <- list(call = match.call())
  ARGS <- list(...)
  # Check object class
  if(!all(sapply(ARGS, function(x) inherits(x, "simRestSelect")))){
    stop("all objects must be of the simRestSelect class")
  }
  # Group
  group <- lapply(ARGS, function(x) x$selection$group)
  group <- lapply(group, data.table::as.data.table, keep.rownames = FALSE)
  group <- data.table::rbindlist(group, use.names = TRUE, fill = TRUE)
  RES$selection$group <- data.frame(group)
  # Composition
  comp <- lapply(ARGS, function(x) x$selection$composition)
  comp <- lapply(comp, data.table::as.data.table, keep.rownames = TRUE)
  comp <- data.table::rbindlist(comp, use.names = TRUE, fill = TRUE)
  compRowNames <- comp$rn
  comp <- as.matrix(comp[,-1])
  rownames(comp) <- compRowNames
  comp[is.na(comp)] <- 0
  RES$selection$composition <- comp
  # Results
  results <- lapply(ARGS, function(x) x$selection$results)
  results <- lapply(results, data.table::as.data.table, keep.rownames = FALSE)
  results <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  RES$selection$results <- as.data.frame(results)
  # Multifunctionality
  multifun <- lapply(ARGS, function(x) x$selection$multifunctionality)
  checkNullMultifun <- sapply(multifun, function(x) is.null(x))
  if(!c(all(checkNullMultifun == TRUE) || all(checkNullMultifun == FALSE))){
    stop("all objects must contain (or none of them must contain) the multifunctionality results")
  }
  if(!all(checkNullMultifun)){
    multifun <- lapply(multifun, data.table::as.data.table, keep.rownames = FALSE)
    multifun <- data.table::rbindlist(multifun, use.names = TRUE, fill = TRUE)
    RES$selection$multifunctionality <- as.data.frame(multifun)
  }
  # The thresholds are not merged/shown
  # Extract reference and supplementary information only for the first object
  x <- ARGS[[1]] 
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
}

#' @rdname simulateCommunities
#' @include simulateCommunities.R
#' @encoding UTF-8
#' @export
mergeSimulations <- function(...) {
  RES <- list(call = match.call())
  ARGS <- list(...)
  # Check object class
  if(!all(sapply(ARGS, function(x) inherits(x, "simRest")))){
    stop("All objects must be of class simRest")
  }
  # Group
  group <- lapply(ARGS, function(x) x$simulation$group)
  group <- lapply(group, data.table::as.data.table, keep.rownames = FALSE)
  group <- data.table::rbindlist(group, use.names = TRUE, fill = TRUE)
  RES$simulation$group <- data.frame(group)
  # Composition
  comp <- lapply(ARGS, function(x) x$simulation$composition)
  comp <- lapply(comp, data.table::as.data.table, keep.rownames = TRUE)
  comp <- data.table::rbindlist(comp, use.names = TRUE, fill = TRUE)
  resRowNames <- comp$rn
  comp <- as.matrix(comp[, -1, drop = FALSE])
  rownames(comp) <- resRowNames
  comp[is.na(comp)] <- 0
  RES$simulation$composition <- comp
  # Baseline
  baseline <- lapply(ARGS, function(x) x$simulation$baseline)
  baseline <- lapply(baseline, data.table::as.data.table, keep.rownames = TRUE)
  baseline <- data.table::rbindlist(baseline, use.names = TRUE, fill = TRUE)
  baseRowNames <- baseline$rn
  baseline <- as.matrix(baseline[, -1, drop = FALSE])
  rownames(baseline) <- baseRowNames
  baseline[is.na(baseline)] <- 0
  RES$simulation$baseline <- baseline
  # Results
  results <- lapply(ARGS, function(x) x$simulation$results)
  checkNull <- sapply(results, function(x) is.null(x))
  if(!c(all(checkNull == TRUE) || all(checkNull == FALSE))){
    stop("Calculated results must be present in either all objects or none")
  }
  if(!all(checkNull)){
    results <- lapply(results, data.table::as.data.table, keep.rownames = FALSE)
    results <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
    RES$simulation$results <- as.data.frame(results)
  }
  # Multifunctionality
  multifun <- lapply(ARGS, function(x) x$simulation$multifunctionality)
  checkNullMultifun <- sapply(multifun, function(x) is.null(x))
  if(!c(all(checkNullMultifun == TRUE) || all(checkNullMultifun == FALSE))){
    stop("Multifunctionality results must be present in either all objects or none")
  }
  if(!all(checkNullMultifun)){
    multifun <- lapply(multifun, data.table::as.data.table, keep.rownames = FALSE)
    multifun <- data.table::rbindlist(multifun, use.names = TRUE, fill = TRUE)
    RES$simulation$multifunctionality <- as.data.frame(multifun)
  }
  # Multisite results
  multisiteRes <- lapply(ARGS, function(x) x$simulation$multisite$results)
  checkNullMultisitesRes <- sapply(multisiteRes, function(x) is.null(x))
  if(!c(all(checkNullMultisitesRes == TRUE) || all(checkNullMultisitesRes == FALSE))){
    stop("Multisite results must be present in either all objects or none")
  }
  if(!all(checkNullMultisitesRes)){
    multisiteRes <- lapply(multisiteRes, data.table::as.data.table, keep.rownames = FALSE)
    multisiteRes <- data.table::rbindlist(multisiteRes, use.names = TRUE, fill = TRUE)
    RES$simulation$multisite$results <- as.data.frame(multisiteRes)
  }
  # Multisite combinations
  multisiteComb <- lapply(ARGS, function(x) x$simulation$multisite$combinations)
  checkNullMultisitesComb <- sapply(multisiteComb, function(x) is.null(x))
  if(!c(all(checkNullMultisitesComb == TRUE) || all(checkNullMultisitesComb == FALSE))){
    stop("Multisite results must be present in either all objects or none")
  }
  if(!all(checkNullMultisitesComb)){
    multisiteComb <- lapply(multisiteComb, data.table::as.data.table, keep.rownames = FALSE)
    multisiteComb <- data.table::rbindlist(multisiteComb, use.names = TRUE, fill = TRUE)
    RES$simulation$multisite$combinations <- as.data.frame(multisiteComb)
  }
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
  if(!is.null(x$reference$multifunctionality)){
    RES$reference$multifunctionality <- x$reference$multifunctionality
  }
  if(!is.null(x$supplementary$multifunctionality)){
    RES$supplementary$multifunctionality <- x$supplementary$multifunctionality
  }
  class(RES) <- "simRest"
  return(RES)
}
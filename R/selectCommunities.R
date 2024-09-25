#' @title Select communities
#' @description Select simulated communities based on tests provided
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @aliases mergeSelection print.simRestSelect
#' @param x A object of class "simRest" or "simRestSelect" to perform communities selection (or additional selection).
#' @param tests A vector with selection criteria to be performed.
#' @param ... Objects of class "simRestSelect" to be concatenated.
#' @returns A list (class "simRestSelect") with the elements:
#' \item{call}{The arguments used.}
#' \item{selection$composition}{A matrix with species composition for selected communities.}
#' \item{selection$group}{A data frame with complementary information for selected sites.}
#' \item{selection$results}{A data frame with calculated parameters in each selected community.}
#' \item{selection$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{selection$thresholds}{A vector with the count of selected communities at each threshold. When simulations are merged, it is not shown.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' @author See \code{\link{CCC-package}}.
#' @seealso \code{\link{checkReference}}, \code{\link{simulateCommunities}}, \code{\link{computeParameters}},
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' head(cerrado.mini$traits)
#' # Simulation
#' scenario <- simulateCommunities(trait = cerrado.mini$traits,
#'                          ava = "Available",
#'                          cwm = "BT",
#'                          rao = c("SLA", "Height", "Seed"),
#'                          rich = c(10, 15),
#'                          it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               trait = cerrado.mini$traits,
#'                     ava = "Available",
#'                     cwm = "BT",
#'                     rao = c("SLA", "Height", "Seed"),
#'                     cost = "Cost",
#'                     dens = "Density",
#'                     reference = cerrado.mini$reference,
#'                     supplementary = cerrado.mini$supplementary)
#' scenario
#' # Select communities
#' scenarioSelected <- selectCommunities(x = scenario,
#'                                       tests = c("CWM_BT > 8",
#'                                                 "rao > 2.5"))
#' scenarioSelected
#' @export 
selectCommunities <- function(x, tests){
  RES <- list(call = match.call())
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    xPar <- x$simulation$results
    comp <- x$simulation$composition
    group <- x$simulation$group
    xMulti <- x$simulation$multifunctionality
  } else{
    xPar <- x$selection$results
    comp <- x$selection$composition
    group <- x$selection$group
    xMulti <- x$selection$multifunctionality
  }
  # Evaluate test
  completeString <- paste0('xPar', '$', tests)
  testsEval <- sapply(completeString, function(a) eval(parse(text=a)))
  pos <- apply(testsEval, 1, all) 
  # Select 
  selPar <- xPar[pos, , drop = FALSE] 
  selCom <- comp[pos, , drop = FALSE]
  selGroup <- group[pos, , drop = FALSE]
  # Multifunctionality
  if(!is.null(xMulti)){
    RES$selection$multifunctionality <- xMulti[pos, , drop = FALSE]
  }
  # Number of selected communities
  nSel <- apply(testsEval, 2, sum)
  names(nSel) <- tests
  nSel <- c(nSel, all = sum(pos))
  # Format thresholds (removed for now)
  # testsSplit <- strsplit(tests, ' ')
  # trsh <- sapply(testsSplit, '[', 3)
  # names(trsh) <- sapply(testsSplit, '[', 1)
  # Set results
  # x$selection$results <- selPar
  # x$selection$composition <- selCom
  # x$selection$group <- group
  # x$selection$N <- nSel
  # x$selection$thresholds <- trsh
  RES$selection$composition <- selCom
  RES$selection$group <- selGroup
  RES$selection$results <- selPar
  # RES$selection$N <- nSel
  # RES$selection$thresholds <- trsh
  RES$selection$thresholds <- nSel
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
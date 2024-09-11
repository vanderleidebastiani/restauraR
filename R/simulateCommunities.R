#' @title Generate simulated communities to restoration
#' @description Generates simulated communities.
#' @details
#' @encoding UTF-8
#' @importFrom data.table rbindlist as.data.table
#' @aliases mergeSimulations print.simRest
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param restComp A matrix with species proportions in the restoration sites. NAs not accepted.
#' @param restGroup Data frame or matrix with complementary information for restoration sites.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param und A vector indicating trait name which indicates undesired species (1 or 0) in trait data.
#' @param it Number of iterations (communities).
#' @param rich The range of richness values in each community.
#' @param cwm A vector with traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist).
#' @param max_add
#' @param min_p
#' @param phi A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy (default phi = 1).
#' @param prefix A prefix to use in current simulation.
#' @param ... Objects of class "simRest" to be concatenated.
#' @returns A list (class "simRest") with the elements:
#' \item{call}{The arguments used.}
#' \item{simulation$composition}{A matrix with species composition for simulated communities.}
#' \item{simulation$group}{A data frame with complementary information for restoration sites.}
#' \item{simulation$results}{A data frame with calculated parameters in each simulated community.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' @author 
#' @seealso
#' @references 
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' head(cerrado.mini$traits)
#' # Restoration new sites
#' scenarioA <- simulateCommunities(trait = cerrado.mini$traits,
#'                          ava = "Available",
#'                          cwm = "BT",
#'                          rao = c("SLA", "Height", "Seed"),
#'                          rich = c(10, 15),
#'                          it = 100)
#' scenarioA
#' # Restoration existing sites
#' scenarioB <- simulateCommunities(trait = cerrado.mini$traits,
#'                     restComp = cerrado.mini$restoration,
#'                     ava = "Available",
#'                     cwm = "BT",
#'                     rao = c("SLA", "Height", "Seed"),
#'                     rich = c(10, 15),
#'                     it = 100,
#'                     max_add = 10)
#' scenarioB
#' # Merge all scenarios
#' allScenarios <- mergeSimulations(scenarioA, scenarioB)
#' allScenarios
#' @export
simulateCommunities <- function(trait, restComp, restGroup, ava, und, it, rich, cwm, rao, max_add, min_p, phi = 1, prefix = NULL){
  RES <- list(call = match.call())
  # Generate species proportions
  propMatrix <- propMatrix(trait = trait, ava = ava, und = und, it = it, 
                           rich = rich, cwm = cwm, rao = rao, phi = phi)
  # Include species proportions in restoration sites
  if(!missing(restComp)){
    rowNameProMatrix <- rownames(propMatrix)
    rowNameRest <- rownames(restComp)
    # template1 <- propMatrix[0,]
    # template2 <- restComp[0,]
    # template0 <- data.table::rbindlist(list(data.table::as.data.table(template1), data.table::as.data.table(template2)), use.names = TRUE, fill = TRUE)
    # propMatrix <- data.table::rbindlist(list(template0, data.table::as.data.table(propMatrix)), use.names = TRUE, fill = TRUE)
    # restComp <- data.table::rbindlist(list(template0, data.table::as.data.table(restComp)), use.names = TRUE, fill = TRUE)
    # propMatrix <- as.matrix(propMatrix)
    # restComp <- as.matrix(restComp)
    # rownames(propMatrix) <- rowNameProMatrix
    # rownames(restComp) <- rowNameRest
    # propMatrix[is.na(propMatrix)] <- 0
    # restComp[is.na(restComp)] <- 0
    template0 <- makeMatrixTemplate(propMatrix, restComp)
    propMatrix <- reorganizeMatrix(template = template0, propMatrix, fillNA = TRUE)
    restComp <- reorganizeMatrix(template = template0, restComp, fillNA = TRUE)
    # TRANSFORM PROPORTIONS AND SUM TO REST 
    propMatrixAdd <- propMatrix * max_add #transforma matriz
    # Set prop = 0 to rare species
    if(!missing(min_p)){
      pos <- propMatrixAdd < min_p
      propMatrixAdd[pos] <- 0
      propMatrixAdd <- (propMatrixAdd/rowSums(propMatrixAdd)) * max_add
    }
    propMatrixList <- apply(restComp, 1, FUN = function(x){ #para cada comun restaurada
      propMatrix_x <- apply(propMatrixAdd, 1, FUN = function(y){ #para cada comun simulada
        x_y <- x + y #restaurada + simulada
        return(x_y)
      })
      propMatrix_x <- t(propMatrix_x)
      propMatrix_x <- propMatrix_x/rowSums(propMatrix_x)
      return(propMatrix_x)
    }, simplify = FALSE)
    propMatrixTab <- do.call(rbind, propMatrixList)
    rownames(propMatrixTab) <- as.vector(t(outer(rowNameRest, rowNameProMatrix, FUN = paste0)))
    restName <- rep(rowNameRest, each = length(rowNameProMatrix))
    if(!missing(restGroup)){
      restGroup <- restGroup[rep(seq_len(nrow(restGroup)), each = length(rowNameProMatrix)),, drop = FALSE]
      restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrixTab)), NAME = restName, restGroup)
    } else{
      restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrixTab)), NAME = restName)
    }
  } else { 
    restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrix)))
    propMatrixTab <- propMatrix
  }
  if(!is.null(prefix)){
      restGroup <- data.frame(PREFIX = prefix, restGroup)
  }
  rownames(propMatrixTab) <- paste0(prefix, rownames(propMatrixTab))
  rownames(restGroup) <- NULL
  RES$simulation$composition <- propMatrixTab
  RES$simulation$group <- restGroup
  class(RES) <- "simRest"
  # Composicao pode ter linhas e/ou colunas com tudo zero. Remover?
  return(RES)
}
#' @title Compute functional parameters in communities
#' @description \code{computeParameters} Calculate basic parameters in each community: richness, count species unavailable, Community Weighted Mean, Community Weighted Variance, Rao Quadratic Entropy and functional dissimilarity.
#' 
#' \code{standardizeParameters} Standardizes the calculated parameters. Two methods are available: "max", which divides the values by the maximum, and "standardise", which scales the calculated parameters to zero mean and unit variance.
#' 
#' \code{computeMultifunctionality} Computes the matrix of multifunctionality and alpha multifunctionality. 
#' @encoding UTF-8
#' @importFrom data.table rbindlist
#' @importFrom SYNCSA matrix.t
#' @importFrom stats dist
#' @aliases computeMultifunctionality standardizeParameters
#' @param x A object of class "simRest" or "simRestSelect" to perform calculate communities parameters.
#' @param trait data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param cwm A vector with traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param cwv A vector with traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.
#' @param rao A vector with traits names to calculate Rao Quadratic Entropy, or distance matrix (class dist). Or a list for calculate multiples Rao.
#' @param cost A vector with trait name with of species cost per individual.
#' @param dens A vector with trait name with species planting density.
#' @param dissimilarity A vector with traits names to calculate dissimilarity with reference sites, or distance matrix (class dist).
#' @param reference A matrix with species proportions in the reference sites. NAs not accepted. (default reference = NULL)
#' @param supplementary A matrix with species proportions in the supplementary sites. NAs not accepted. (default supplementary = NULL).
#' @param tests A vector with multifunctionality criteria to be performed. 
#' @param parameters A vector with parameters names to standardized.
#' @param method Standardization method, "max" or "standardize". 
#' @returns A list (class "simRest" or "simRestSelect") with the elements:
#' \item{call}{The arguments used.}
#' \item{simulation$composition}{A matrix with species composition for simulated communities.}
#' \item{simulation$group}{A data frame with complementary information for restoration sites.}
#' \item{simulation$results}{A data frame with calculated parameters in each simulated community.}
#' \item{simulation$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{reference$multifunctionality}{A data frame with binary multifunctionality tests to reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' \item{supplementary$multifunctionality}{A data frame with binary multifunctionality tests to supplementary sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{checkReference}}, \code{\link{simulateCommunities}}, \code{\link{selectCommunities}},
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
#'                               ava = "Available",
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               cost = "Cost",
#'                               dens = "Density",
#'                               dissimilarity = c("SLA", "Height", "Seed"),
#'                               reference = cerrado.mini$reference,
#'                               supplementary = cerrado.mini$supplementary)
#' scenario
#' # standardize parameters
#' scenario <- standardizeParameters(x = scenario, 
#'                                   parameters = "dissimilarity",
#'                                   method = "max")
#' scenario
#' # Compute multifunctionality
#' scenario <- computeMultifunctionality(x = scenario, 
#'                                  tests = c("CWM_BT > 8",
#'                                            "rao > 2.5"))
#' scenario
#' @export
computeParameters <- function(x, trait, ava = NULL, cwm = NULL, cwv = NULL, rao = NULL, cost = NULL, dens = NULL, dissimilarity = NULL, reference = NULL, supplementary = NULL){
  # Check object class
  if(!inherits(x, "simRest")){
    stop("x must be of the simRest class")
  }
  composition <- x$simulation$composition
  nSim <- nrow(composition)
  traitsNames <- colnames(trait)
  # Merge compositions - simulations, reference and supplementary
  if(!is.null(reference) && is.null(supplementary)){
    nRef <- nrow(reference)
    template0 <- makeMatrixTemplate(composition, reference)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition)
    x$reference$composition <- reference
  }
  if(!is.null(supplementary) && is.null(reference)){
    nSupple <- nrow(supplementary)
    template0 <- makeMatrixTemplate(composition, supplementary)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    supplementary <- reorganizeMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(composition, supplementary)
    x$supplementary$composition <- supplementary
  }
  if(!is.null(reference) && !is.null(supplementary)){
    nRef <- nrow(reference)
    nSupple <- nrow(supplementary)
    template0 <- makeMatrixTemplate(composition, reference, supplementary)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    supplementary <- reorganizeMatrix(template = template0, supplementary, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition, supplementary)
    x$reference$composition <- reference
    x$supplementary$composition <- supplementary
  }
  # Calculate species proportions
  composition <- sweep(composition, MARGIN = 1, rowSums(composition), FUN = "/")
  # Organize traits
  matchNames <- match(colnames(composition), rownames(trait))
  trait <- as.data.frame(trait[matchNames, , drop = FALSE])
  # Calculate parameters
  out <- NULL
  # Count species unavailable
  if(!is.null(ava)){
    if(!inherits(ava, 'character') || !all(ava %in% traitsNames) || length(ava)>1){
      stop("ava must be a character indicating a single column of the trait data frame")
    }
    UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(trait[,ava])] > 0) )
    out <- cbind(out, unavailable = UNA)
  }
  # Richness
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  # Simpson diversity
  diverSimpson <- SYNCSA::rao.diversity(comm = composition)$Simpson
  out <- cbind(out, simpson = diverSimpson)
  # CWM
  if(!is.null(cwm)){
    if(!inherits(cwm, 'character') || !all(cwm %in% traitsNames)){
      stop("cwm must be a character indicating one or more columns of the trait data frame")
    }
    traitSub <- trait[, cwm, drop = FALSE]
    CWM <- SYNCSA::matrix.t(composition, traitSub, scale = FALSE)$matrix.T
    colnames(CWM) <- paste0("CWM_", colnames(CWM))
    out <- cbind(out, CWM)
  }
  # CWV
  if(!is.null(cwv)){
    if(!inherits(cwv, 'character') || !all(cwv %in% traitsNames)){
      stop("cwv must be a character indicating one or more columns of the trait data frame")
    }
    traitSub <- trait[, cwv, drop = FALSE]
    CWV <- calcCWV(composition, traitSub)
    colnames(CWV) <- paste0("CWV_", colnames(CWV))
    out <- cbind(out, CWV)
  }
  # Rao diversity
  if(!is.null(rao)){
    # If a list
    if(inherits(rao, 'list')){
      RAOlist <- NULL
      for(i in 1:length(rao)){
        if(inherits(rao[[i]], 'character')){
          if(!all(rao[[i]] %in% traitsNames)){
            stop("each value of rao list must be a character indicating one or more columns of the trait data frame, or distance matrix")
          }
          traitSub <- scale(trait[, rao[[i]], drop = FALSE] )
          # RAOtemp <- fundiversity::fd_raoq(traitSub, composition)$Q
          dis <- stats::dist(traitSub)
          RAOtemp <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(dis))$PhyRao
        } else if(inherits(rao[[i]], 'dist')){
          # RAOtemp <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao[[i]])$Q
          RAOtemp <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(rao[[i]]))$PhyRao
        }
        RAOlist <- cbind(RAOlist, RAOtemp)
      }
      if(is.null(names(rao))){
        colnames(RAOlist) <- paste0("rao_", seq_len(length(rao)))
      } else{
        colnames(RAOlist) <- paste0("rao_", names(rao))
      }
      out <- cbind(out, RAOlist)
    } else{ # If a vector
      if(inherits(rao, 'character')){
        if(!all(rao %in% traitsNames)){
          stop("rao must be a character indicating one or more columns of the trait data frame, or distance matrix, or a list")
        }
        traitSub <- scale(trait[, rao, drop = FALSE] )
        dis <- stats::dist(traitSub)
        # RAO <- fundiversity::fd_raoq(traitSub, composition)$Q
        RAO <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(dis))$PhyRao
      } else if(inherits(rao, 'dist')){
        # RAO <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao)$Q
        RAO <- SYNCSA::rao.diversity(comm = composition, phylodist = as.matrix(rao))$PhyRao
      }
      out <- cbind(out, rao = RAO)
    }
  }
  # Cost - It require species cost and planting density
  if(!is.null(cost) || !is.null(dens)){
    if(!inherits(cost, 'character') || !all(cost %in% traitsNames) || length(cost)>1){
      stop("cost must be a character indicating a single column of the trait data frame")
    }
    if(!inherits(dens, 'character') || !all(dens %in% traitsNames) || length(dens)>1){
      stop("dens must be a character indicating a single column of the trait data frame")
    }
    costVect <- trait[, cost]
    densVect <- trait[, dens]
    COST <- apply(composition, 1, FUN = function(p){
      COST_i <- sum(p*costVect*densVect, na.rm = TRUE)
      return(COST_i)
    })
    out <- cbind(out, cost = COST)
  }
  #  Dissimilatity
  if(!is.null(reference)){
    nRef <- nrow(reference)
    resDis <- calcRAO(composition)
    # Remove matrix diagonal
    diag(resDis) <- NA
    # Keep only values related to reference sites
    resDis <- resDis[, seq.int(nRef), drop = FALSE]
    # Calculate mean dissimilarities
    resDis <- apply(resDis, MARGIN = 1, mean, na.rm = TRUE)
    out <- cbind(out, dissimilarity = resDis)
  }
  # Functional dissimilatity
  if(!is.null(dissimilarity) && !is.null(reference)){
    nRef <- nrow(reference)
    if(inherits(dissimilarity, 'character')){
      if(!all(dissimilarity %in% traitsNames)){
        stop("dissimilarity must be a character indicating one or more columns of the trait data frame, or distance matrix")
      }
      traitSub <- scale(trait[, dissimilarity, drop = FALSE])
      dis <- stats::dist(traitSub)
      # resDis <- as.matrix(adiv::discomQE(composition, dis = dis, formula = "QE"))
      resDis <- calcRAO(composition, dis = dis)
    } else if(inherits(dissimilarity, 'dist')){
      # Calculate dissimilarities between communities
      # resDis <- as.matrix(adiv::discomQE(composition, dis = dissimilarity, formula = "QE"))
      resDis <- calcRAO(composition, dis = dissimilarity)
    }
    # Remove matrix diagonal
    diag(resDis) <- NA
    # Keep only values related to reference sites
    resDis <- resDis[, seq.int(nRef), drop = FALSE]
    # Calculate mean dissimilarities
    resDis <- apply(resDis, MARGIN = 1, mean, na.rm = TRUE)
    out <- cbind(out, functionalDissimilarity = resDis)
  }
  # Results organization
  if(!is.null(reference) || !is.null(supplementary)){
    if(!is.null(reference) && is.null(supplementary)){
      x$reference$results <- cbind.data.frame(out[seq.int(nRef), , drop = FALSE])
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[-1*seq.int(nRef), , drop = FALSE])  
    }
    if(!is.null(supplementary) && is.null(reference)){
      nTotal <- nrow(out)
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[seq.int(nTotal-nSupple), , drop = FALSE])  
      x$supplementary$results <- cbind.data.frame(out[-1*seq.int(nTotal-nSupple), , drop = FALSE])
    }
    if(!is.null(reference) && !is.null(supplementary)){
      nTotal <- nrow(out)
      x$reference$results <- cbind.data.frame(out[seq.int(nRef), , drop = FALSE])
      x$simulation$results <- cbind.data.frame(x$simulation$group, out[seq.int(nTotal)[(nRef+1):(nSim+nRef)], , drop = FALSE])
      x$supplementary$results <- cbind.data.frame(out[seq.int(nTotal)[(nRef+nSim+1):nTotal], , drop = FALSE])
    }
  } else {
    x$simulation$results <- cbind.data.frame(x$simulation$group, out)  
  }
  rownames(x$simulation$results) <- NULL
  return(x)
}
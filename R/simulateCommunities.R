#' @title function to generate simulated communities (comSimulation)
#' @description generates simulated communities and calculate its parameters.
#' @details
#' @encoding UTF-8
#' @importFrom Select selectSpecies
#' @importFrom fundiversity fd_raoq
#' @importFrom data.table rbindlist
#' @aliases
#' @param 
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
simulateCommunities <- function(trait, restComp, restGroup, ava, und, it, rich, cwm, rao, max_add, min_p, phi = 1, prefix = NULL){
  # RES <- vector("list")
  RES <- list(call = match.call())
  # Generate species proportions
  propMatrix <- propMatrix(trait = trait, ava = ava, und = und, it = it, 
                           rich = rich, cwm = cwm, rao = rao, phi = phi)
  # Include species proportions in restoration sites
  if(!missing(restComp)){
    rowNameProMatrix <- rownames(propMatrix)
    rowNameRest <- rownames(restComp)
    template1 <- propMatrix[0,]
    template2 <- restComp[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    propMatrix <- data.table::rbindlist(list(template0, as.data.table(propMatrix)), use.names = TRUE, fill = TRUE)
    restComp <- data.table::rbindlist(list(template0, as.data.table(restComp)), use.names = TRUE, fill = TRUE)
    propMatrix <- as.matrix(propMatrix)
    restComp <- as.matrix(restComp)
    rownames(propMatrix) <- rowNameProMatrix
    rownames(restComp) <- rowNameRest
    propMatrix[is.na(propMatrix)] <- 0
    restComp[is.na(restComp)] <- 0
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
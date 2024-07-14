#' @title function to generate simulated communities
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
comSimulation <- function(trait, ava, und, it, rich, cwm, rao, rest, max_add, min_p, phi = 1, restGroup, prefix = NULL){
  
  # trait = dados$trait[80:120,]
  # ava = dados$ava
  # und = dados$und
  # it = dados$it
  # rich = c(10, 12)
  # cwm = dados$cwm
  # rao = dados$cwm
  # rest = dados$rest
  # restGroup = dados$restGroup
  # max_add = dados$max_add
  # min_p = dados$min_p
  # phi = 1
  # prefix = "Ongoing"

  
  RES <- vector("list")
  # RES$sim$parameters$trait <- trait
  # RES$sim$parameters$ava <- ava
  # RES$sim$parameters$cwm <- cwm
  # RES$sim$parameters$rao <- rao
  propMatrix <- propMatrix(trait = trait, ava = ava, und = und, it = it, 
                           rich = rich, cwm = cwm, rao = rao, phi = phi)
  
  # rest <- NULL
  # restGroup <- NULL
  # if(!is.null(restGroup) && is.null(rest)){
  #   rest <- matrix(0, nrow(restGroup), ncol = nrow(trait))
  #   rownames(rest) <- rownames(restGroup)
  #   colnames(rest) <- rownames(trait)
  # }
  if(!missing(rest)){
    rowNameProMatrix <- rownames(propMatrix)
    rowNameRest <- rownames(rest)
    template1 <- propMatrix[0,]
    template2 <- rest[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    # dim(template2)
    # setdiff(colnames(propMatrix), colnames(rest))
    propMatrix <- data.table::rbindlist(list(template0, as.data.table(propMatrix)), use.names = TRUE, fill = TRUE)
    rest <- data.table::rbindlist(list(template0, as.data.table(rest)), use.names = TRUE, fill = TRUE)
    propMatrix <- as.matrix(propMatrix)
    rest <- as.matrix(rest)
    rownames(propMatrix) <- rowNameProMatrix
    rownames(rest) <- rowNameRest
    propMatrix[is.na(propMatrix)] <- 0
    rest[is.na(rest)] <- 0
    # TRANSFORM PROPORTIONS AND SUM TO REST #################
    propMatrixAdd <- propMatrix * max_add #transforma matriz
    #Set prop = 0 to rare species: <<<<<<<<<<<<<<<<<<<<<<<<<
    if(!missing(min_p)){
      pos <- propMatrixAdd < min_p
      propMatrixAdd[pos] <- 0
      propMatrixAdd <- (propMatrixAdd/rowSums(propMatrixAdd)) * max_add
      # pos <- is.na(propMatrixAdd[,1])
      # propMatrixAdd <- propMatrixAdd[!pos,]
    }
    propMatrixList <- apply(rest, 1, FUN = function(x){ #para cada comun restaurada
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
      # restGroup <- rep(restGroup, each = length(rowNameProMatrix))  
      restGroup <- restGroup[rep(seq_len(nrow(restGroup)), each = length(rowNameProMatrix)),, drop = FALSE]
      restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrixTab)), NAME = restName, restGroup)
      # rownames(restGroup) <- paste0(prefix, rownames(propMatrixTab))
    } else{
      restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrixTab)), NAME = restName)
      # rownames(restGroup) <- paste0(prefix, rownames(propMatrixTab))
    }
  } else { 
    # restName <- NULL
    # restGroup <- NULL
    restGroup <- data.frame(SIM = paste0(prefix, rownames(propMatrixTab)))
    propMatrixTab <- propMatrix
  }
  if(!is.null(prefix)){
    # if(!is.null(restGroup)){
      restGroup <- data.frame(PREFIX = prefix, restGroup)
    # } else{
    #   restGroup <- data.frame(PREFIX = rep(prefix, nrow(propMatrixTab)))
    #   rownames(restGroup) <- rownames(propMatrixTab)
    # }
  }
  rownames(propMatrixTab) <- paste0(prefix, rownames(propMatrixTab))
  rownames(restGroup) <- NULL
  RES$sim$composition <- propMatrixTab
  # RES$sim$restName <- restName
  RES$sim$restGroup <- restGroup
  # RES$sim$prefix <- prefix
  # Composicao pode ter linhas e/ou colunas com tudo zero. Remover?
  return(RES)
}
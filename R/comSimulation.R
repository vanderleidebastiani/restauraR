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
comSimulation <- function(trait, ava, und, it, rich, cwm, rao, rest, max_add, min_p, phi = 1){
  RES <- vector("list")
  RES$sim$parameters$trait <- trait
  RES$sim$parameters$ava <- ava
  RES$sim$parameters$cwm <- cwm
  RES$sim$parameters$rao <- rao
  propMatrix <- propMatrix(trait = trait, ava = ava, und = und, it = it, 
                           rich = rich, cwm = cwm, rao = rao, phi = phi)
  # propMatrix <- propMatrix(trait = trait, ava = ava, it = it, 
  # 						 rich = rich, cwm = cwm, cwv = cwv, rao = rao, phi = phi)
  # ATE AQUI NA FUNCAO propMatrix ----
  if(!missing(rest)){
    template1 <- propMatrix[0,]
    template2 <- rest[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    propMatrix <- data.table::rbindlist(list(template0, as.data.table(propMatrix)), use.names = TRUE, fill = TRUE)
    rest <- data.table::rbindlist(list(template0, as.data.table(rest)), use.names = TRUE, fill = TRUE)
    propMatrix <- as.matrix(propMatrix)
    rest <- as.matrix(rest)
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
    propMatrixList <- apply(rest, 1, FUN=function(x){ #para cada comun restaurada
      propMatrix_x <- apply(propMatrixAdd, 1, FUN = function(y){ #para cada comun simulada
        x_y <- x + y #restaurada + simulada
        return(x_y)
      })
      propMatrix_x <- t(propMatrix_x)
      propMatrix_x <- propMatrix_x/rowSums(propMatrix_x)
      return(propMatrix_x)
    }, simplify = FALSE)
    propMatrixTab <- do.call(rbind, propMatrixList)
  } else{ propMatrixTab <- propMatrix}
  RES$sim$composition <- propMatrixTab
  return(RES)
}
#' @title checkReference
#' @description checkReference
#' @details
#' @encoding UTF-8
#' @aliases
#' @param ref
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
checkReference <- function(ref, trait, cwm, cwv, rao, stan, supplementary = NULL){
  RES <- list()
  x <- list()
  x$sim$composition <- ref
  x$sim$restGroup <- data.frame(NAMES = rownames(ref))
  temp <- calcPar(x, trait = trait, cwm = cwm, cwv = cwv, rao = rao, stan = stan, supplementary = supplementary)
  # temp <- calcPar(x, trait = trait, cwm = cwm, cwv = cwv, rao = rao, supplementary = supplementary)
  
  if(!is.null(supplementary)){
        RES$reference$results <- temp$sim$results[,-1, drop = FALSE]
        RES$supplementary$results <- temp$supplementary$results

    } else {

      RES$reference$results <- temp$sim$results[,-1, drop = FALSE]
    }
  
  # composition <- ref
  # nSim <- nrow(composition)
  # 
  # if(!is.null(supplementary)){
  #   rowNameComposition <- rownames(composition)
  #   rowNameSupple <- rownames(supplementary)
  #   nSupple <- nrow(supplementary)
  #   template1 <- composition[0,]
  #   template2 <- supplementary[0,]
  #   template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
  #   composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
  #   supplementary <- data.table::rbindlist(list(template0, as.data.table(supplementary)), use.names = TRUE, fill = TRUE)
  #   composition <- as.matrix(composition)
  #   supplementary <- as.matrix(supplementary)
  #   rownames(composition) <- rowNameComposition
  #   rownames(supplementary) <- rowNameSupple
  #   composition[is.na(composition)] <- 0
  #   supplementary[is.na(supplementary)] <- 0
  #   composition <- rbind(composition, supplementary)
  #   # x$supplementary$composition <- supplementary
  # }
  # 
  # out <- NULL
  # S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  # out <- cbind(out, richness = S)
  # 
  # if(!missing(cwm)){
  #   if(inherits(cwm, 'character')){
  #     traitSub <- trait[,cwm, drop = FALSE]
  #     CWM <- SYNCSA::matrix.t(composition, traitSub, scale = FALSE)$matrix.T
  #     out <- cbind(out, CWM)
  #   }
  # }
  # 
  # if(!missing(cwv)){
  #   if(inherits(cwv, 'character')){
  #     traitSub <- trait[,cwv, drop=FALSE]
  #     CWV <- FCWV(composition, traitSub)
  #     out <- cbind(out, CWV)
  #   }
  # }
  # 
  # if(!missing(rao)){
  #   if(inherits(rao, 'character')){
  #     traitSub <- scale(trait[,rao, drop=FALSE] )
  #     RAO <- fundiversity::fd_raoq(traitSub, composition)$Q
  #   } else if(inherits(rao, 'dist')){
  #     RAO <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao)$Q
  #   } 
  #   out <- cbind(out, rao = RAO)
  # }
  # 
  # if(!missing(stan)){
  #   out[ , stan] <- out[ , stan, drop = FALSE]/max(out[ , stan, drop = FALSE])
  # } 
  # 
  # 
  # if(!is.null(supplementary)){
  #     nTotal <- nrow(out)
  #     
  #     RES$reference$results <- cbind.data.frame(out[seq.int(nTotal-nSupple),])
  #     RES$supplementary$results <- out[-1*seq.int(nTotal-nSupple),]
  #   
  # } else {
  #   
  #   RES$reference$results <- cbind.data.frame(out)  
  # }
  return(RES)
}
calcPar <- function(x){
  
  # und, cost, dens,
  # stan, ref, rest
  
  
  # ATE AQUI NA FUNCAO comSimulation ----
  # CALCULATE PARAMETERS ##################################
  # TEMP
  
  composition <- x$sim$composition
  trait <- x$sim$parameters$trait
  ava <- x$sim$parameters$ava
  cwm <- x$sim$parameters$cwm
  rao <- x$sim$parameters$rao
  cwv <- x$sim$parameters$cwv
  cost <- x$sim$parameters$cost
  
  out <- NULL
  
  #rest <- rest/rowSums(rest)
  # x <- rbind(propMatrixTab, rest)
  # 
  # if(!missing(ref)){
  #   ref <- ref/rowSums(ref)
  #   x <- rbind(x, ref)
  # } else {x <- x}
  
  UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(trait[,ava])] > 0) )
  out <- cbind(out, unavailable = UNA)
  
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  
  if(!is.null(cwm)){
    if(inherits(cwm, 'character')){
      traitSub <- trait[,cwm, drop=FALSE]
      CWM <- matrix.t(composition, traitSub, scale = FALSE)$matrix.T
      out <- cbind(out, CWM)
    }
  }
  
  if(!is.null(cwv)){
    if(inherits(cwv, 'character')){
      traitSub <- trait[,cwv, drop=FALSE]
      CWV <- FCWV(composition, traitSub)
      out <- cbind(out, CWV)
    }
  }
  
  if(!is.null(rao)){
    if(inherits(rao, 'character')){
      traitSub <- scale(trait[,rao, drop=FALSE] )
      RAO <- fundiversity::fd_raoq(traitSub, composition)$Q
    } else if(inherits(rao, 'dist')){
      RAO <- fundiversity::fd_raoq(sp_com = composition, dist_matrix = rao)$Q
    } 
    #RAO <- RAO/max(RAO) #$$$$$$$$$$$$
    out <- cbind(out, rao = RAO)
    # colnames(out)[colnames(out) == 'rao'] <- rao
  }
  
  # if(!is.null(cost)){
  #   COST <- apply(x, 1, FUN=function(p){
  #     COST_i <- sum(p*cost*dens, na.rm = TRUE)
  #     return(COST_i)
  #   })
  #   out <- cbind(out, cost = COST)
  # }
  # 
  # if(!missing(stan)){
  #   out[,stan] <- out[,stan,drop=F]/max(out[,stan,drop=F])
  # } #$$$$$$$$$$$$$$$$$$$
  # 
  # if(!missing(ref)){
  #   f <- c(rep(row.names(rest),each = nrow(propMatrixList$X1) ), #posicao comunidades restauradas modificadas
  #          rep('restored', nrow(rest)), #posicao comunidades restauradas
  #          rep('reference', nrow(ref)) #posicao comunidades ref
  #   )
  # } else {
  #   f <- c(rep(row.names(rest),each = nrow(propMatrixList$X1)  ), #posicao comunidades restauradas modificadas
  #          rep('restored', nrow(rest)), #posicao comunidades restauradas
  #   )
  # } #https://stackoverflow.com/questions/13060000/how-can-i-separate-a-matrix-into-smaller-ones-in-r
  # 
  # pos <- duplicated(colnames(out)) | duplicated(colnames(out), fromLast=TRUE)
  # dup_col <- colnames(out)[pos]
  # correct_col <- paste0(dup_col, 1:length(dup_col)) #add index to duplicated
  # colnames(out)[pos] <- correct_col
  # 
  # outList <- lapply( split( out, f ), matrix, ncol=ncol(out))
  # for(i in 1:length(outList)){
  #   colnames(outList[[i]]) <- colnames(out)
  #   row.names(outList[[i]]) <- sprintf("sim%d",seq(1:nrow(outList[[i]])))
  # }
  # 
  # if(!missing(ref)){
  #   propMatrixList2 <- c(propMatrixList, list(restored = rest), list(reference = ref) )
  # } else {
  #   propMatrixList2 <- c(propMatrixList, list(restored = rest) )
  # }
  # 
  # ordem <- match(names(propMatrixList2), names(outList))
  # outList <- outList[ordem]
  # all(names(outList) == names(propMatrixList2))
  # 
  # out2 <- list(compositions = propMatrixList2, parameters = outList)
  x$sim$results <- out
  return(x)
}
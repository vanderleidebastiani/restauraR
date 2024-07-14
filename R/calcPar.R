# x = RES1
# trait = dados$trait
# cwm = dados$cwm
# rao = dados$cwm
# cwv = dados$cwm
# ava = dados$ava
# ref = dados$ref[1:10,]
# supplementary = dados$ref[11:19,]



# x <- allSim
# trait = dados$trait
# cwm = dados$cwm
# rao = dados$cwm
# cwv = dados$cwm
# ava = dados$ava
# ref = dados$ref[1:10,]
# supplementary = dados$ref[11:19,]




calcPar <- function(x, trait, ava, cwm, cwv, rao, cost, dens, stan, ref = NULL, supplementary = NULL){
  
  # cost, dens,
  # stan, ref, rest
  # x <- RES1
  # ATE AQUI NA FUNCAO comSimulation ----
  # CALCULATE PARAMETERS ##################################
  # TEMP
  composition <- x$sim$composition
  nSim <- nrow(composition)
  if(!is.null(ref) && is.null(supplementary)){
    rowNameComposition <- rownames(composition)
    rowNameRef <- rownames(ref)
    nRef <- nrow(ref)
    template1 <- composition[0,]
    template2 <- ref[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    ref <- data.table::rbindlist(list(template0, as.data.table(ref)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    ref <- as.matrix(ref)
    rownames(composition) <- rowNameComposition
    rownames(ref) <- rowNameRef
    composition[is.na(composition)] <- 0
    ref[is.na(ref)] <- 0
    composition <- rbind(ref, composition)
    x$ref$composition <- ref
  }
  if(!is.null(supplementary) && is.null(ref)){
    rowNameComposition <- rownames(composition)
    rowNameSupple <- rownames(supplementary)
    nSupple <- nrow(supplementary)
    template1 <- composition[0,]
    template2 <- supplementary[0,]
    template0 <- data.table::rbindlist(list(as.data.table(template1), as.data.table(template2)), use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    supplementary <- data.table::rbindlist(list(template0, as.data.table(supplementary)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    supplementary <- as.matrix(supplementary)
    rownames(composition) <- rowNameComposition
    rownames(supplementary) <- rowNameSupple
    composition[is.na(composition)] <- 0
    supplementary[is.na(supplementary)] <- 0
    composition <- rbind(composition, supplementary)
    x$supplementary$composition <- supplementary
  }
  if(!is.null(ref) && !is.null(supplementary)){
    rowNameComposition <- rownames(composition)
    rowNameRef <- rownames(ref)
    rowNameSupple <- rownames(supplementary)
    nRef <- nrow(ref)
    nSupple <- nrow(supplementary)
    template0 <- list(composition[0,], ref[0,], supplementary[0,])
    template0 <- lapply(template0, as.data.table, keep.rownames = TRUE)
    template0 <- data.table::rbindlist(template0, use.names = TRUE, fill = TRUE)
    composition <- data.table::rbindlist(list(template0, as.data.table(composition)), use.names = TRUE, fill = TRUE)
    ref <- data.table::rbindlist(list(template0, as.data.table(ref)), use.names = TRUE, fill = TRUE)
    supplementary <- data.table::rbindlist(list(template0, as.data.table(supplementary)), use.names = TRUE, fill = TRUE)
    composition <- as.matrix(composition)
    ref <- as.matrix(ref)
    supplementary <- as.matrix(supplementary)
    rownames(composition) <- rowNameComposition
    rownames(ref) <- rowNameRef
    rownames(supplementary) <- rowNameSupple
    composition[is.na(composition)] <- 0
    ref[is.na(ref)] <- 0
    supplementary[is.na(supplementary)] <- 0
    composition <- rbind(ref, composition, supplementary)
    x$ref$composition <- ref
    x$supplementary$composition <- supplementary
  }
  # composition <- x$sim$composition
  # trait <- x$sim$parameters$trait
  # ava <- x$sim$parameters$ava
  # cwm <- x$sim$parameters$cwm
  # rao <- x$sim$parameters$rao
  # cwv <- x$sim$parameters$cwv
  # cost <- x$sim$parameters$cost
  
  out <- NULL
  
  #rest <- rest/rowSums(rest)
  # x <- rbind(propMatrixTab, rest)
  # 
  
  if(!missing(ava)){
    if(inherits(ava, 'character')){
      UNA <- apply(composition, 1, FUN = function(a) sum(a[!as.logical(trait[,ava])] > 0) )
      out <- cbind(out, unavailable = UNA)
    }
  }
  
  S <- apply(composition, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  
  if(!missing(cwm)){
    if(inherits(cwm, 'character')){
      traitSub <- trait[,cwm, drop = FALSE]
      CWM <- SYNCSA::matrix.t(composition, traitSub, scale = FALSE)$matrix.T
      out <- cbind(out, CWM)
    }
  }
  
  if(!missing(cwv)){
    if(inherits(cwv, 'character')){
      traitSub <- trait[,cwv, drop=FALSE]
      CWV <- FCWV(composition, traitSub)
      out <- cbind(out, CWV)
    }
  }
  
  if(!missing(rao)){
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
  
  if(!missing(cost)){
    COST <- apply(x, 1, FUN=function(p){
      COST_i <- sum(p*cost*dens, na.rm = TRUE)
      return(COST_i)
    })
    out <- cbind(out, cost = COST)
  }
  
  if(!missing(stan)){
    out[ , stan] <- out[ , stan, drop = FALSE]/max(out[ , stan, drop = FALSE])
  } 
  
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
  if(!is.null(ref) || !is.null(supplementary)){
    if(!is.null(ref) && is.null(supplementary)){
      x$ref$results <- out[seq.int(nRef),]
      # x$sim$results <- out[-1*seq.int(nRef),]
      # if(!is.null(x$sim$restName)){
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, restName = x$sim$restName, out[-1*seq.int(nRef),])  
      # } else{
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, out[-1*seq.int(nRef),])  
      # }
      x$sim$results <- cbind.data.frame(x$sim$restGroup, out[-1*seq.int(nRef),])  
    }
    if(!is.null(supplementary) && is.null(ref)){
      nTotal <- nrow(out)
      # x$sim$results <- out[seq.int(nTotal-nSupple),]
      # if(!is.null(x$sim$restName)){
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, restName = x$sim$restName, out[seq.int(nTotal-nSupple),])  
      # } else{
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, out[seq.int(nTotal-nSupple),])  
      # }
      x$sim$results <- cbind.data.frame(x$sim$restGroup, out[seq.int(nTotal-nSupple),])  
      x$supplementary$results <- out[-1*seq.int(nTotal-nSupple),]
    }
    if(!is.null(ref) && !is.null(supplementary)){
      nTotal <- nrow(out)
      x$ref$results <- out[seq.int(nRef),]
      # x$sim$results <- out[seq.int(nTotal)[(nRef+1):(nSim+nRef)],]
      # if(!is.null(x$sim$restName)){
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, restName = x$sim$restName, out[seq.int(nTotal)[(nRef+1):(nSim+nRef)],])
      # } else{
      #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, out[seq.int(nTotal)[(nRef+1):(nSim+nRef)],])  
      # }
      x$sim$results <- cbind.data.frame(x$sim$restGroup, out[seq.int(nTotal)[(nRef+1):(nSim+nRef)],])
      x$supplementary$results <- out[seq.int(nTotal)[(nRef+nSim+1):nTotal],]
    }
  } else {
    # x$sim$results <- out  
    # if(!is.null(x$sim$restName)){
    #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, restName = x$sim$restName, out)  
    # } else{
    #   x$sim$results <- cbind.data.frame(prefix = x$sim$prefix, out)  
    # }
    x$sim$results <- cbind.data.frame(x$sim$restGroup, out)  
  }
  return(x)
}
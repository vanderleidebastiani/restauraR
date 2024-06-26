findSpecies <- function(trait, rao, cwm, rich, phi){
  nSpeciesInt <- nrow(trait)
  species <- rownames(trait)
  if(inherits(rao, 'character')){
    t2d <- as.matrix(scale(trait[, rao, drop = FALSE]) )
  } else if(inherits(rao, 'dist')){
    t2d <- rao
  }
  if(!missing(cwm)){
    t2c <- as.matrix(trait[,cwm, drop = FALSE])
    constraints <- apply(t2c, 2, function(x){
      cons <- seq(min(x), max(x), length.out = 8)
      return(cons[c(-1,-8)])
    })
    selSpp <- vector(mode="list", length = length(cwm))
    names(selSpp) <- cwm
    for(i in cwm){
      cons_i <- constraints[,i]
      t2c_i <- t2c[,i, drop=FALSE]
      # selSpp_i <- c()
      selSpp_i <- matrix(NA, nrow = nSpeciesInt, ncol = 6)
      for(j in 1:length(cons_i)){
        # names(j) <- colnames(t2c_i)
        # invisible(capture.output(selSpp_j <- Select::selectSpecies(t2c_i, j, t2d, phi = phi) ) ) 
        # selSpp_i <- cbind(selSpp_i, selSpp_j$prob)
        jj <- cons_i[j]
        names(jj) <- colnames(t2c_i)
        invisible(capture.output(selSpp_j <- Select::selectSpecies(t2c_i, jj, t2d, phi = phi) ) )
        selSpp_i[,j] <- selSpp_j$prob
      }
      selSpp[[i]] <- selSpp_i 
    }
    propMatrixSelSpp <- do.call(cbind, selSpp)
    sppMax <- c()
    propMin <- 0.1
    while(length(sppMax) < rich[1]){
      sppMax <- which(propMatrixSelSpp > propMin, arr.ind = TRUE)
      sppMax <- unique(species[sppMax[,1]])
      propMin <- 0.5*propMin
    }
    # propMatrixSelSpp <- round(t(propMatrixSelSpp),3)
  } else {
    # CONFERIR obj ----
    invisible(capture.output( selSpp <- Select::selectSpecies(t2d = t2d, phi = phi, obj = "QH")$prob ))
    propMatrixSelSpp <- selSpp
    sppMax <- c()
    propMin <- 0.1
    while(length(sppMax) < rich[1]){
      sppMax <- species[propMatrixSelSpp > propMin]
      propMin <- 0.5*propMin
    }
  }
  return(sppMax)
}



propMatrix <- function(trait, ava, und, it, rich, cwm, rao, phi){
  # Set prop = 0 to undesired species: <<<<<<<<<<<<<<<<<<<<
  if(!missing(und)){
    undLog <- as.logical(trait[,und])
    trait <- trait[!undLog,] # remove undesired species
  }
  # Create random communities
  nSpecies <- nrow(trait)
  species <- row.names(trait)
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  sampleAbundance <- function(nRich1, nRich2, vLen){
    nsp_i <-  resample(nRich1:nRich2, 1)
    ocor <- sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
    abund <- rlnorm(vLen)
    abund <- abund * ocor
    prop <- abund/sum(abund)
    return(prop)
  }
  # Find distant species: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  if(!missing(rao)){
    # if(inherits(rao, 'character')){
    #   t2d <- as.matrix(scale(trait[, rao, drop = FALSE]) )
    # } else if(inherits(rao, 'dist')){
    #   t2d <- rao
    # }
    # if(!missing(cwm)){
    #   t2c <- as.matrix(trait[,cwm, drop = FALSE])
    #   constraints <- apply(t2c, 2, function(x){
    #     cons <- seq(min(x), max(x), length.out = 8)
    #     return(cons[c(-1,-8)])
    #   })
    #   selSpp <- vector(mode="list", length = length(cwm))
    #   names(selSpp) <- cwm
    #   for(i in cwm){
    #     cons_i <- constraints[,i]
    #     t2c_i <- t2c[,i, drop=FALSE]
    #     # selSpp_i <- c()
    #     selSpp_i <- matrix(NA, nrow = nSpecies, ncol = 6)
    #     for(j in 1:length(cons_i)){
    #       # names(j) <- colnames(t2c_i)
    #       # invisible(capture.output(selSpp_j <- Select::selectSpecies(t2c_i, j, t2d, phi = phi) ) ) 
    #       # selSpp_i <- cbind(selSpp_i, selSpp_j$prob)
    #       jj <- cons_i[j]
    #       names(jj) <- colnames(t2c_i)
    #       invisible(capture.output(selSpp_j <- Select::selectSpecies(t2c_i, jj, t2d, phi = phi) ) )
    #       selSpp_i[,j] <- selSpp_j$prob
    #     }
    #     selSpp[[i]] <- selSpp_i 
    #   }
    #   propMatrixSelSpp <- do.call(cbind, selSpp)
    #   sppMax <- c()
    #   propMin <- 0.1
    #   while(length(sppMax) < rich[1]){
    #     sppMax <- which(propMatrixSelSpp > propMin, arr.ind = TRUE)
    #     sppMax <- unique(species[sppMax[,1]])
    #     propMin <- 0.5*propMin
    #   }
    #   # propMatrixSelSpp <- round(t(propMatrixSelSpp),3)
    # } else {
    #   invisible( capture.output( selSpp <- Select::selectSpecies(t2d = t2d, phi = phi, obj = "QH")$prob ))
    #   propMatrixSelSpp <- selSpp
    #   sppMax <- c()
    #   propMin <- 0.1
    #   while(length(sppMax) < rich[1]){
    #     sppMax <- species[propMatrixSelSpp > propMin]
    #     propMin <- 0.5*propMin
    #   }
    # }
    
    sppMax <- findSpecies(trait, rao, cwm, rich, phi)
    
    # Find distant species that are available: <<<<<<<<<<<<<<
    # avaLog <- as.logical(ava)
    ava2 <- trait[,ava]
    avaLog <- as.logical(trait[,ava])
    speciesAva <- species[avaLog]
    
    # if(inherits(rao, 'character')){
    #   t2d <- as.matrix(scale(trait[avaLog, rao, drop = FALSE]) )
    # } else if(inherits(rao, 'dist')){
    #   t2d <- as.dist(as.matrix(rao)[avaLog, avaLog])
    # }
    # if(!missing(cwm)){
    #   t2c <- as.matrix(trait[avaLog, cwm, drop = FALSE])
    #   constraints <- apply(t2c, 2, function(x){
    #     cons <- seq(min(x), max(x), length.out = 8)
    #     return(cons[c(-1,-8)])
    #   })
    #   selSppAva <- vector(mode="list", length=length(cwm))
    #   names(selSppAva) <- cwm
    #   for(i in cwm){
    #     cons_i <- constraints[,i]
    #     t2c_i <- t2c[,i, drop=FALSE]
    #     selSppAva_i <- c()
    #     for(j in cons_i){
    #       names(j) <- colnames(t2c_i)
    #       invisible(capture.output(selSppAva_j <- Select::selectSpecies(t2c_i, j, t2d, phi = phi) ))
    #       selSppAva_i <- cbind(selSppAva_i, selSppAva_j$prob)
    #     }
    #     selSppAva[[i]] <- selSppAva_i 
    #   }
    #   propMatrixSelSppAva <- do.call(cbind, selSppAva)
    #   sppMaxAva <- c()
    #   propMin <- 0.1
    #   while(length(sppMaxAva) < rich[1]){ 
    #     sppMaxAva <- which(propMatrixSelSppAva > propMin, arr.ind = TRUE)
    #     sppMaxAva <- unique(speciesAva[sppMaxAva[,1]])
    #     propMin <- 0.5*propMin
    #   }
    #   # propMatrixSelSppAva <- round(t(propMatrixSelSppAva),3)
    # } else {
    #   invisible(capture.output(selSppAva <- Select::selectSpecies(t2d = t2d, phi = phi)$prob ))
    #   propMatrixSelSppAva <- selSpp
    #   sppMaxAva <- c() 
    #   propMin <- 0.1 
    #   while(length(sppMaxAva) < rich[1]){
    #     sppMaxAva <- species[propMatrixSelSppAva > propMin]
    #     propMin <- 0.5*propMin
    #   }
    # }
    
    sppMaxAva <- findSpecies(trait[speciesAva,], rao, cwm, rich, phi)
    
    # number of iterations for simulations: <<<<<<<<<<<<<<<<<
    itMax <- round(0.25*it)
    itMaxAva <- round(0.25*it)
    itAva <- round(0.25*it)
    itAll <- it - itMax - itMaxAva - itAva
    
    # Run simulation with species that maximize rao: <<<<<<<<<
    if(length(sppMax) < rich[2]){
      nsp <- length(sppMax)
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- length(sppMax)
    }
    propMatrixSelSpp2 <- matrix(0, ncol=nSpecies, nrow=itMax)
    sppMaxPos <- species %in% sppMax
    for(i in 1:itMax){
      # nsp_i <-  resample(rich[1]:nsp, 1)
      # ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
      # abund = rlnorm(vLen)
      # abund <- abund * ocor
      # prop <- abund/sum(abund)
      # propMatrixSelSpp2[i,sppMaxPos] <- prop
      propMatrixSelSpp2[i,sppMaxPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
    }
    
    # Run simulation with species that maximize rao and are available: <<<<
    if(length(sppMaxAva) < rich[2]){
      nsp <- length(sppMaxAva) 
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- length(sppMaxAva)
    }
    propMatrixSelSppAva2 <- matrix(0, ncol=nSpecies, nrow=itMaxAva)
    sppMaxAvaPos <- species %in% sppMaxAva
    for(i in 1:itMaxAva){
      # nsp_i <-  resample(rich[1]:nsp, 1)
      # ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
      # abund = rlnorm(vLen)
      # abund <- abund * ocor
      # prop <- abund/sum(abund)
      # propMatrixSelSppAva2[i,sppMaxAvaPos] <- prop
      propMatrixSelSppAva2[i,sppMaxAvaPos] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
    }
  } else {
    avaLog <- as.logical(ava2)
    itAva <- round(0.5*it)
    itAll <- it - itAva
  }
  
  # ATE AQUI IGUAL
  
  
  # Run simulation with available species: <<<<<<<<<<<<<<<<
  if(sum(ava2) < rich[2]){
    nsp <- sum(ava2) 
    vLen <- nsp
  } else {
    nsp <- rich[2]
    vLen <- sum(ava2)
  }
  propMatrixAva <- matrix(0,
                          ncol=nSpecies, nrow=itAva)
  if(rich[1] > nsp){
    stop("Minimum richness is higher than number of available species")
  }
  for(i in 1:itAva){
    # nsp_i <-  resample(c(rich[1]:nsp), 1)
    # ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
    # abund = rlnorm(vLen)
    # abund <- abund * ocor
    # prop <- abund/sum(abund)
    # propMatrixAva[i,avaLog] <- prop
    propMatrixAva[i,avaLog] <- sampleAbundance(nRich1 = rich[1], nRich2 = nsp, vLen = vLen)
  }
  
  # Run simulation with all species: <<<<<<<<<<<<<<<<<<<<<<
  # nsp <- length(species)
  propMatrixPool <- matrix(0,
                           ncol=nSpecies, nrow=itAll)
  for(i in 1:itAll){
    # nsp_i <-  resample(c(rich[1]:rich[2]), 1)
    # ocor <- sample( c(rep(1, nsp_i), rep(0, nsp - nsp_i)) )
    # abund <- rlnorm(nsp)
    # abund <- abund * ocor
    # prop <- abund/sum(abund)
    # propMatrixPool[i,] <- prop
    propMatrixPool[i,] <- sampleAbundance(nRich1 = rich[1], nRich2 = rich[2], vLen = nSpecies)
  }
  
  # Bind all matrices: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  
  if(!missing(rao)){
    propMatrix <- rbind(propMatrixSelSpp2,propMatrixSelSppAva2,
                        propMatrixAva, propMatrixPool)
  } else {
    propMatrix <- rbind(propMatrixAva, propMatrixPool)
  }
  
  
  rownames(propMatrix) <- sprintf("sim%d",seq(1:nrow(propMatrix)))
  colnames(propMatrix) <- species
  
  # Set prop = 0 to undesired species: <<<<<<<<<<<<<<<<<<<<
  # if(!missing(und)){
  #   pos <- as.logical(und)
  #   propMatrix[,pos] <- 0
  #   pos <- rowSums(propMatrix) == 0 #se ==0 simulacao contem apenas und
  #   propMatrix <- propMatrix[!pos,] #elimina linhas sem spp
  #   propMatrix <- propMatrix/rowSums(propMatrix)
  # }
  
  
  row.names(propMatrix) <- sprintf("sim%d",seq(1:nrow(propMatrix)))
  
  return(propMatrix)
}


# CMV
FCWV <- function(x, traitSub){
  temp <- SYNCSA::matrix.t(x, traitSub, scale = FALSE)
  MW <- temp$matrix.w
  MCWM <- temp$matrix.T
  MCWV <- matrix(NA, nrow(MW), ncol(MCWM))
  for(i in 1:nrow(MW)){
    for(j in 1:ncol(MCWM)){
      MCWV[i, j] <- sum(MW[i,]*(traitSub[,j]-MCWM[i,j])^2)
    }
  }
  rownames(MCWV) <- rownames(MCWM)
  colnames(MCWV) <- colnames(MCWM)
  return(MCWV)
}

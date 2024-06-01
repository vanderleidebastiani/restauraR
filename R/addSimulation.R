#' @title function to generate simulated communities to add on ongoing restoration
#' @description generates simulated communities and calculate its parameters.
#' @details
#' @encoding UTF-8
#' @importFrom
#' @aliases
#' @param trait data frame or matrix with species traits. Traits as columns and species as rows.
#' @param ava vector indicating availability of species (1 or 0)
#' @param und vector indicating undesired species (1 or 0)
#' @param it number of iterations (communities) for each area ongoing restoration
#' @param rich range of richness values
#' @param cwm vector with traits to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.
#' @param rao vector with traits to calculate Rao Quadratic Entropy, or distance matrix (class dist)
#' @param cost vector of species cost per individual
#' @param dens vector of planting density
#' @param stan which parameters should be standardized by the maximum? Must provide trait names
#' @param ref matrix with proportion of species in the reference sites. NAs not accepted.
#' @param rest matrix with proportion of species in the restored sites. NAs not accepted.
#' @param max_add maximum proportion that can be added to restored areas.
#' @param min_p minimum species proportion to consider that species occurs in the area
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
addSimulation <- function(trait, ava, und, it, rich, cwm, cwv, rao, cost, dens,
                          stan, ref, rest, max_add, min_p, phi = 1){
  # CREATE RANDOM COMMUNITIES #############################
  species <- row.names(trait)
  resample <- function(x, ...) x[sample.int(length(x), ...)]
  
  # Find distant species: <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  if(!missing(rao)){
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
      selSpp <- vector(mode="list", length=length(cwm))
      names(selSpp) <- cwm
      for(i in cwm){
        cons_i <- constraints[,i]
        t2c_i <- t2c[,i, drop=FALSE]
        selSpp_i <- c()
        for(j in cons_i){
          names(j) <- colnames(t2c_i)
          invisible(capture.output(selSpp_j <- selectSpecies(t2c_i, j,
                                                             t2d, phi = phi) ) ) 
          selSpp_i <- cbind(selSpp_i, selSpp_j$prob)
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
      propMatrixSelSpp <- round(t(propMatrixSelSpp),3)
    } else {
      invisible( capture.output( selSpp <- selectSpecies(t2d = t2d, phi = phi)$prob ))
      propMatrixSelSpp <- selSpp
      sppMax <- c()
      propMin <- 0.1
      while(length(sppMax) < rich[1]){
        sppMax <- species[propMatrixSelSpp > propMin]
        propMin <- 0.5*propMin
      }
    }
    
    # Find distant species that are available: <<<<<<<<<<<<<<
    avaLog <- as.logical(ava)
    speciesAva <- species[avaLog]
    if(inherits(rao, 'character')){
      t2d <- as.matrix(scale(trait[avaLog, rao, drop = FALSE]) )
    } else if(inherits(rao, 'dist')){
      t2d <- as.dist(as.matrix(rao)[avaLog, avaLog])
    }
    if(!missing(cwm)){
      t2c <- as.matrix(trait[avaLog, cwm, drop = FALSE])
      constraints <- apply(t2c, 2, function(x){
        cons <- seq(min(x), max(x), length.out = 8)
        return(cons[c(-1,-8)])
      })
      selSppAva <- vector(mode="list", length=length(cwm))
      names(selSppAva) <- cwm
      for(i in cwm){
        cons_i <- constraints[,i]
        t2c_i <- t2c[,i, drop=FALSE]
        selSppAva_i <- c()
        for(j in cons_i){
          names(j) <- colnames(t2c_i)
          invisible(capture.output(selSppAva_j <- selectSpecies(t2c_i, j, t2d, phi = phi) ))
          selSppAva_i <- cbind(selSppAva_i, selSppAva_j$prob)
        }
        selSppAva[[i]] <- selSppAva_i 
      }
      propMatrixSelSppAva <- do.call(cbind, selSppAva)
      sppMaxAva <- c()
      propMin <- 0.1
      while(length(sppMaxAva) < rich[1]){ 
        sppMaxAva <- which(propMatrixSelSppAva > propMin, arr.ind = TRUE)
        sppMaxAva <- unique(speciesAva[sppMaxAva[,1]])
        propMin <- 0.5*propMin
      }
      propMatrixSelSppAva <- round(t(propMatrixSelSppAva),3)
    } else {
      invisible(capture.output(selSppAva <- selectSpecies(t2d = t2d, phi = phi)$prob ))
      propMatrixSelSppAva <- selSpp
      sppMaxAva <- c() 
      propMin <- 0.1 
      while(length(sppMaxAva) < rich[1]){
        sppMaxAva <- species[propMatrixSelSppAva > propMin]
        propMin <- 0.5*propMin
      }
    }
    
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
    propMatrixSelSpp2 <- matrix(rep(0,length(species)*itMax),
                                ncol=length(species), nrow=itMax)
    sppMaxPos <- species %in% sppMax
    for(i in 1:itMax){
      nsp_i <-  resample(rich[1]:nsp, 1)
      ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
      abund = rlnorm(vLen)
      abund <- abund * ocor
      prop <- abund/sum(abund)
      propMatrixSelSpp2[i,sppMaxPos] <- prop
    }
    
    # Run simulation with species that maximize rao and are available: <<<<
    if(length(sppMaxAva) < rich[2]){
      nsp <- length(sppMaxAva) 
      vLen <- nsp
    } else {
      nsp <- rich[2]
      vLen <- length(sppMaxAva)
    }
    propMatrixSelSppAva2 <- matrix(rep(0,length(species)*itMaxAva),
                                   ncol=length(species), nrow=itMaxAva)
    sppMaxAvaPos <- species %in% sppMaxAva
    for(i in 1:itMaxAva){
      nsp_i <-  resample(rich[1]:nsp, 1)
      ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
      abund = rlnorm(vLen)
      abund <- abund * ocor
      prop <- abund/sum(abund)
      propMatrixSelSppAva2[i,sppMaxAvaPos] <- prop
    }
  } else {
    avaLog <- as.logical(ava)
    itAva <- round(0.5*it)
    itAll <- it - itAva
  }
  
  # Run simulation with available species: <<<<<<<<<<<<<<<<
  if(sum(ava) < rich[2]){
    nsp <- sum(ava) 
    vLen <- nsp
  } else {
    nsp <- rich[2]
    vLen <- sum(ava)
  }
  propMatrixAva <- matrix(rep(0,length(species)*itAva),
                          ncol=length(species), nrow=itAva)
  if(rich[1] > nsp){
    stop("Minimum richness is higher than number of available species")
  }
  for(i in 1:itAva){
    nsp_i <-  resample(c(rich[1]:nsp), 1)
    ocor = sample( c(rep(1, nsp_i), rep(0, vLen - nsp_i)) )
    abund = rlnorm(vLen)
    abund <- abund * ocor
    prop <- abund/sum(abund)
    propMatrixAva[i,avaLog] <- prop
  }
  
  # Run simulation with all species: <<<<<<<<<<<<<<<<<<<<<<
  nsp <- length(species)
  propMatrixPool <- matrix(rep(0,length(species)*itAll),
                           ncol=length(species), nrow=itAll)
  for(i in 1:itAll){
    nsp_i <-  resample(c(rich[1]:rich[2]), 1)
    ocor = sample( c(rep(1, nsp_i), rep(0, nsp - nsp_i)) )
    abund = abund = rlnorm(nsp)
    abund <- abund * ocor
    prop <- abund/sum(abund)
    propMatrixPool[i,] <- prop
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
  if(!missing(und)){
    pos <- as.logical(und)
    propMatrix[,pos] <- 0
    pos <- rowSums(propMatrix) == 0 #se ==0 simulacao contem apenas und
    propMatrix <- propMatrix[!pos,] #elimina linhas sem spp
    propMatrix <- propMatrix/rowSums(propMatrix)
  }
  
  row.names(propMatrix) <- sprintf("sim%d",seq(1:nrow(propMatrix)))
  
  # TRANSFORM PROPORTIONS AND SUM TO REST #################
  propMatrixAdd <- propMatrix * max_add #transforma matriz
  #Set prop = 0 to rare species: <<<<<<<<<<<<<<<<<<<<<<<<<
  if(!missing(min_p)){
    pos <- propMatrixAdd < min_p
    propMatrixAdd[pos] <- 0
    propMatrixAdd <- (propMatrixAdd/rowSums(propMatrixAdd)) * max_add
    pos <- is.na(propMatrixAdd[,1])
    propMatrixAdd <- propMatrixAdd[!pos,]
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
  
  # CALCULATE PARAMETERS ##################################
  out <- NULL
  
  #rest <- rest/rowSums(rest)
  x <- rbind(propMatrixTab, rest)
  
  if(!missing(ref)){
    ref <- ref/rowSums(ref)
    x <- rbind(x, ref)
  } else {x <- x}
  
  UNA <- apply(x, 1, FUN = function(a) sum(a[!as.logical(ava)] > 0) )
  out <- cbind(out, unavailable = UNA)
  
  S <- apply(x, 1, FUN = function(a) sum(a > 0))
  out <- cbind(out, richness = S)
  
  if(!missing(cwm)){
    if(inherits(cwm, 'character')){
      traitSub <- trait[,cwm, drop=FALSE]
      CWM <- apply(x, 1, FUN=function(p){
        colSums(traitSub*p, na.rm = T)
      })
      if(class(CWM)[1] == 'matrix'){
        CWM <- t(CWM)
      } else {
        CWM <- as.matrix(CWM)
      }
      #colnames(CWM) <- paste0('cwm_', cwm)
      out <- cbind(out, CWM)
    } else {message('** CWM skipped. **')}
  }
  
  if(!missing(cwv)){
    if(inherits(cwv, 'character')){
      traitSub <- trait[,cwv, drop=FALSE]
      CWM <- apply(x, 1, FUN=function(p){
        colSums(traitSub*p, na.rm = T)
      })
      if(class(CWM)[1] == 'matrix'){
        CWM <- t(CWM)
      } else {
        CWM <- as.matrix(CWM)
      }
      
      CWV <- NULL
      for(i in 1:nrow(CWM)){ #para cada linha de CWMs
        CWM_i <- CWM[i,] #linha i
        traitMod <- NULL #traits modificados pelo CWM i
        for(j in 1:ncol(traitSub)){
          traitMod_j <- traitSub[,j] - CWM_i[j] #subtrai pelo CWM
          traitMod_j <- traitMod_j^2 #eleva ao quadrado
          traitMod <- cbind(traitMod, traitMod_j) #adiciona na tabela
        }
        #CWV: traits modificados pelo CWM i vezes proporcao i
        CWV_i <- colSums(traitMod*x[i,])
        CWV <- rbind(CWV, CWV_i)
      }
      #colnames(CWV) <- paste0('cwv_', cwv)
      colnames(CWV) <- cwv
      #CWV <- apply(CWV, 2, FUN = function(x){x/max(x)}) #padroniza pelo maximo #$$$$$$$
      out <- cbind(out, CWV)
    } else {message('** CWV skipped. **')}
  }
  
  if(!missing(rao)){
    if(inherits(rao, 'character')){
      traitSub <- scale(trait[,rao, drop=FALSE] )
      RAO <- fd_raoq(traitSub, x)$Q
    } else if(inherits(rao, 'dist')){
      RAO <- fd_raoq(sp_com = x, dist_matrix = rao)$Q
    } else{
      message('** RAO skipped')
    }
    #RAO <- RAO/max(RAO) #$$$$$$$$$$$$
    out <- cbind(out, rao = RAO)
    colnames(out)[colnames(out) == 'rao'] <- rao
  }
  
  if(!missing(cost)){
    COST <- apply(x, 1, FUN=function(p){
      COST_i <- sum(p*cost*dens, na.rm = TRUE)
      return(COST_i)
    })
    out <- cbind(out, cost = COST)
  }
  
  if(!missing(stan)){
    out[,stan] <- out[,stan,drop=F]/max(out[,stan,drop=F])
  } #$$$$$$$$$$$$$$$$$$$
  
  if(!missing(ref)){
    f <- c(rep(row.names(rest),each = nrow(propMatrixList$X1) ), #posicao comunidades restauradas modificadas
           rep('restored', nrow(rest)), #posicao comunidades restauradas
           rep('reference', nrow(ref)) #posicao comunidades ref
    )
  } else {
    f <- c(rep(row.names(rest),each = nrow(propMatrixList$X1)  ), #posicao comunidades restauradas modificadas
           rep('restored', nrow(rest)), #posicao comunidades restauradas
    )
  } #https://stackoverflow.com/questions/13060000/how-can-i-separate-a-matrix-into-smaller-ones-in-r
  
  pos <- duplicated(colnames(out)) | duplicated(colnames(out), fromLast=TRUE)
  dup_col <- colnames(out)[pos]
  correct_col <- paste0(dup_col, 1:length(dup_col)) #add index to duplicated
  colnames(out)[pos] <- correct_col
  
  outList <- lapply( split( out, f ), matrix, ncol=ncol(out))
  for(i in 1:length(outList)){
    colnames(outList[[i]]) <- colnames(out)
    row.names(outList[[i]]) <- sprintf("sim%d",seq(1:nrow(outList[[i]])))
  }
  
  if(!missing(ref)){
    propMatrixList2 <- c(propMatrixList, list(restored = rest), list(reference = ref) )
  } else {
    propMatrixList2 <- c(propMatrixList, list(restored = rest) )
  }
  
  ordem <- match(names(propMatrixList2), names(outList))
  outList <- outList[ordem]
  all(names(outList) == names(propMatrixList2))
  
  out2 <- list(compositions = propMatrixList2, parameters = outList)
  
  return(out2)
}
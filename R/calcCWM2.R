calcCWM2 <- function(comm, traits){
  CWM <- apply(comm, 1, FUN=function(p){
    cwm_p <- colSums(traits*p, na.rm = T)
    return(cwm_p)
  })
  if(class(CWM)[1] == 'matrix'){
    CWM <- t(CWM)
  } else {
    CWM <- as.matrix(CWM)
  }
  return(CWM)
}
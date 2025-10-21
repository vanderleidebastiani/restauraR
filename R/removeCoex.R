removeCoex <- function(x, y, inv){
  #x: vector of final species abundances, to check invalid species pairs
  #y: vector of added species list to eliminate invalid species pairs
  #inv: data.frame with invalid species pairs
  
  #value: logical test indicating species that must be removed
  
  z <- x-y
  z_spp <- names(z)[z>0]
  x_spp <- names(x)[x>0]
  x_spp_pairs <- expand.grid(Species1 = x_spp, Species2 = x_spp)
  x_spp_pairs$x <- 1
  inv_x <- merge(inv, x_spp_pairs, by.x = c("Species1", "Species2"), by.y = c("Species1", "Species2"), all.x = TRUE)
  inv_x <- inv_x[!is.na(inv_x$x) & !is.na(inv_x$x),1:2]
  #inv_x contains invalid species pairs. If both species are already present in z, both must be removed in y. If only one is present in z, the other must be removed in y. If both are not present in z, both can, but only one must be removed in y.
  if(nrow(inv_x) > 0){
    inv_x <- as.matrix(inv_x)
    remov <- NULL
    for(i in 1:nrow(inv_x)){
      spp1 <- inv_x[i,1]
      spp2 <- inv_x[i,2]
      inv_x1 <- inv_x[i,1:2]
      if(all(inv_x1 %in% z_spp)){ #if all exist in z
        remov <- c(remov, inv_x1)
      } else if(any(inv_x1 %in% z_spp)){ #if any exist in z
        remov_i <- inv_x1[!inv_x1%in%z_spp]
        remov <- c(remov, remov_i)
      } else if(!any(inv_x1 %in% z_spp)){ #if none exist in z
        if(any(inv_x1 %in% remov)){ #if any exists in remov
          remov_i <- unique(remov[remov%in%inv_x1])
          remov_i <- remov[sample(1:length(remov_i),1)]
          remov <- c(remov, remov_i)
        } else if(!any(inv_x1 %in% remov)){ #if none exist in remov
          remov_i <- inv_x1[sample(1:2,1)]
          remov <- c(remov, remov_i)
        }
      }
      remov <- unique(remov)
    }
    testTemp <- names(y) %in% remov
  } else {
    testTemp <- rep(FALSE, length(x))
  }
  
  return(testTemp)
}
#' @rdname computeParameters
#' @include computeParameters.R
#' @encoding UTF-8
#' @export
computeDissimilarity <- function(x, trait){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    comp <- x$simulation$composition
  } else{
    comp <- x$selection$composition
  }
  ref <- x$reference$composition
	if(inherits(trait, 'data.frame') || inherits(trait, 'matrix')){
		dis <- stats::dist(scale(trait))
	} else if(inherits(trait, 'dist')){
		dis <- trait
	}
  message('*** Calculating functional dissimilarity. This may take a while. ***')
	i = 0
	DISSIM <- apply(ref, 1, FUN = function(r){
		i <<-  i+1 
		message(paste('##### Reference site number: ',i, '#####') )
		j = 1
		pb <- utils::txtProgressBar(min = 0, max = nrow(comp), style = 3)
		DISSIM_i <- apply(comp, 1, FUN = function(p){ 
		  utils::setTxtProgressBar(pb, j)
			j <<- j +1
			comm <- rbind(r, p)
			DISSIM_j <- adiv::discomQE(comm, dis, formula = "QE")
			return(DISSIM_j)
		})
		close(pb)
		return(DISSIM_i)
	})
	DISSIM <- apply(DISSIM, 1, mean)
	DISSIM <- DISSIM/max(DISSIM)
	if(inherits(x, "simRest")){
	  x$simulation$results$dissimilarity <- DISSIM
	} else{
	  x$selection$results$dissimilarity <- DISSIM
	}
	return(x)
}
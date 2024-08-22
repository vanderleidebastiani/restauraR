#' @rdname calculateParameters
#' @include calculateParameters.R
#' @encoding UTF-8
#' @export
calculateDissimilarity <- function(x, trait){
  # where = "global"
  # argOptions <- c("selection", "global")
  # where <- pmatch(where, argOptions)
  # if (length(where) > 1 || any(is.na(where))) {
  #   stop("Invalid where argument\n")
  # }
  # if(where == 1){ # if selection
  #   comp <- x$selection$composition
  # } else{
  #   comp <- x$simulation$composition
  # }
  if(inherits(x, "simRest")){
    comp <- x$simulation$composition
  } else{
    comp <- x$selection$composition
  }
  ref <- x$reference$composition
	if(inherits(trait, 'data.frame') | inherits(trait, 'matrix')){
		dis <- dist(scale(trait))
	} else if(inherits(trait, 'dist')){
		dis <- trait
	}
  message('*** Calculating functional dissimilarity. This may take a while. ***')
	i = 0
	DISSIM <- apply(ref, 1, FUN = function(r){
		i <<-  i+1 
		message(paste('##### Reference site number: ',i, '#####') )
		j = 1
		pb <- txtProgressBar(min = 0, max = nrow(comp), style = 3)
		DISSIM_i <- apply(comp, 1, FUN = function(p){ 
			setTxtProgressBar(pb, j)
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
	# if(where == 1){ # if selection
	#   x$selection$results$dissimilarity <- DISSIM
	# } else{
	#   x$simulation$results$dissimilarity <- DISSIM
	# }
	if(inherits(x, "simRest")){
	  x$simulation$results$dissimilarity <- DISSIM
	} else{
	  x$selection$results$dissimilarity <- DISSIM
	}
	return(x)
}
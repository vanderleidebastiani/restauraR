#' @title function to calculate functional dissimilarity (distCalculation)
#' @description Calculate functional dissimilarity between simulated communities and reference sites
#' @details
#' @encoding UTF-8
#' @importFrom adiv discomQE
#' @aliases
#' @param sim data frame or matrix with species proportions in simulated communities. Species as columns and simulated communities as rows.
#' @param trait data frame or matrix with species traits. Traits as columns and species as rows.
#' @param dis matrix of trait distances
#' @param ref data frame or matrix with species proportions in reference sites. Species as columns and simulated communities as rows.
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
calculateDissimilarity <- function(x, trait, where = "global"){
  argOptions <- c("selection", "global")
  where <- pmatch(where, argOptions)
  if (length(where) > 1 || any(is.na(where))) {
    stop("Invalid where argument\n")
  }
  if(where == 1){ # if selection
    comp <- x$selection$composition
  } else{
    comp <- x$simulation$composition
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
	if(where == 1){ # if selection
	  x$selection$results$dissimilarity <- DISSIM
	} else{
	  x$simulation$results$dissimilarity <- DISSIM
	}
	return(x)
}
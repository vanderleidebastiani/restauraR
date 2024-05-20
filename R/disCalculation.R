#' @title function to calculate functional dissimilarity
#' @description Calculate functional dissimilarity between simulated communities and reference sites
#' @details
#' @encoding UTF-8
#' @importFrom
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
disCalculation <- function(sim, trait, dis, ref){
	if(inherits(trait, 'data.frame') | inherits(trait, 'matrix')){
		dis <- dist(scale(trait))
		message('*** Calculating functional dissimilarity. This may take a while. ***')
	} else if(inherits(trait, 'dist')){
		dis <- trait
		message('*** Calculating functional dissimilarity. This may take a while. ***')
	}
	i = 0
	DISSIM <- apply(ref, 1, FUN=function(r){
		i <<-  i+1 
		message(paste('##### Reference site number: ',i, '#####') )
		j = 1
		pb <- txtProgressBar(min = 0, max = nrow(sim), style = 3)
		DISSIM_i <- apply(sim, 1, FUN=function(p){ 
			setTxtProgressBar(pb, j)
			j <<- j +1
			comm <- rbind(r, p)
			DISSIM_j <- discomQE(comm, dis, formula = "QE")
			return(DISSIM_j)
		})
		close(pb)
		return(DISSIM_i)
	})
	DISSIM <- apply(DISSIM, 1, mean)
	DISSIM <- DISSIM/max(DISSIM)
	
	return(DISSIM)
	
}
#' @title function to visualize results
#' @description Visualize parameters of simulated and selected communities and reference sites
#' @details
#' @encoding UTF-8
#' @importFrom
#' @aliases
#' @param x name of the variable in x axis. Must match a column in sim and sel_sim
#' @param y name of the variable in the y axis. Must match a column in sim and sel_sim
#' @param sim list with results of function comSelection.
#' @param cols colors of simulated communities, in this order: communities with at least one unavailable species; communities with only available species; communities with at least one unavailable species that satisfy restoration criteria; communities with only available species that satisfy restoration criteria.
#' @param xlab x axis label
#' @param ylab y axis label
#' @param xlim x axis limits
#' @param ylim y axis limits
#' @param hide_una: hide simulated communities with unavailable species?
#' @param hide_notsel: hide simulated communities not selected?
#' @param hide_ref: hide reference sites?
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
viewResults <- function(x, y, sim,
						cols = c('grey', 'black', '#A6CEE3', '#1F78B4', '#B2DF8A'),
						xlab, ylab,
						legend,
						xlim = range(sim$sim_communities$parameters[,x]),
						ylim = range(sim$sim_communities$parameters[,y]),
						hide_notsel = FALSE, hide_una = FALSE,
						hide_ref = FALSE, hide_sel = FALSE){
	all <- sim$sim_communities$parameters
	if(!hide_ref){
		ref <- sim$ref_communities$parameters
	}
	if(!hide_sel){
		sel <- sim$sel_communities$parameters 
	}
	
	# Define xlim e ylim:
	if(missing(xlim)){
		xlim <- range(all[,x])
	}
	if(missing(ylim)){
		ylim <- range(all[,y])
	}
	
	par(mfrow=c(1,2), mar=c(5.1, 1, 4.1, 0), oma=c(0,3,0,0))
	plot(0,0, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
		 col=rgb(0,0,0,alpha=0))
	mtext(ylab,2,3)
	
	#ALL:
	if(!hide_notsel & !hide_una){
		points(all[,x], all[,y], pch = 19, col=cols[1])
	}
	
	#AVA NOT SEL:
	if(!hide_notsel){
		pos <- all$unavailable==0 #posicao de ava
		points(all[pos,x], all[pos,y], pch=19, col=cols[2])
	} 
	
	#SELECTED:
	if(!hide_sel){
		if(!hide_una){
			points(sel[,x], sel[,y], pch=19, col=cols[3])
			pos <- sel$unavailable == 0 
			points(sel[pos,x], sel[pos,y], pch = 19, col=cols[4])
		} else {
			pos <- sel$unavailable == 0
			points(sel[pos,x], sel[pos,y], pch = 19, col=cols[4])
		}
	}
	
	#REF:
	if(!hide_ref){
		points(ref[,x], ref[,y], pch=19, col=cols[5], cex=1.5)
	}
	
	tsh <- sim$thresholds
	abline(v = tsh[x], lty=2, col=cols[4])
	abline(h = tsh[y], lty=2, col=cols[4])
	
	legend <- c("Unavailable",
				"Available",
				"Unavailable - selected",
				"Available - selected",
				"References")
	pos <- c(!hide_una & !hide_notsel,
			 !hide_notsel,
			 !hide_una & !hide_sel,
			 !hide_sel,
			 !hide_ref)
	
	plot.new()
	legend("topleft", col = cols[pos], cex = 1, pch=19,
		   legend=legend[pos])
}

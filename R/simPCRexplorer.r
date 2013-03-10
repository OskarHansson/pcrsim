################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: first version.

#' @title Explore PCR simulator
#'
#' @description
#' \code{simPCRexplorer} explore \code{simPCR} by varying a parameter.
#'
#' @details
#' Makes it easy to explore the effect of one parameter on the PCR simulation.
#' 
#' @param ncells number of cells.
#' @param probEx probability of extraction.
#' @param probAlq probability of aliquote.
#' @param probPCR probability of PCR (PCR efficiency).
#' @param cyc number of PCR cycles.
#' @param tDetect detection threshold.
#' @param KH scaling factor for number of molecules to peak height.
#' @param simulations number of simulations.
#' @param dip logical flagging for diploid cells (haploid cells are currently not implemented)
#' 
#' @return list with simulation results.
#' 
#' @export
#' @examples
#' # Simulating 0-1000 cells 1000 times each and plotting means.
#' sim <- simPCRexplorer(ncells=seq(0,1001, by=10), probEx=0.7, probAlq=0.1, probPCR=0.85)
#' plot(x=seq(0,1001, by=10), y=lapply(sim, mean), xlab=c("# cells"), ylab=c("Peak height (rfu)"))

simPCRexplorer <- function(ncells=100, probEx=1, probAlq=1, probPCR=1,
			cyc=28, tDetect=2*10^7, dip=TRUE, KH=55,simulations=1000){

	# Create variables.
	res<-list()
	parLength <- vector()

	# Get length of all parameter vectors.
	parLength[1] <- length(ncells)
	parLength[2] <- length(probEx)
	parLength[3] <- length(probAlq)
	parLength[4] <- length(probPCR)
	parLength[5] <- length(cyc)
	parLength[6] <- length(tDetect)
	parLength[7] <- length(dip)
	parLength[8] <- length(KH)
	parLength[9] <- length(simulations)

	# It is allowed to vary one parameter only.
	if(sum(parLength>1)>1){
		stop("Only one parameter at a time can be explored!", call. = TRUE)
	}

	# Get vector length.
	vectLen <- max(parLength) 

	# Vectorize all to the same length.
	if(length(ncells) == 1){
		ncells <- rep(ncells, vectLen)
	}
	if(length(probEx) == 1){
		probEx <- rep(probEx, vectLen)
	}
	if(length(probAlq) == 1){
		probAlq <- rep(probAlq, vectLen)
	}
	if(length(probPCR) == 1){
		probPCR <- rep(probPCR, vectLen)
	}
	if(length(cyc) == 1){
		cyc <- rep(cyc, vectLen)
	}
	if(length(tDetect) == 1){
		tDetect <- rep(tDetect, vectLen)
	}
	if(length(dip) == 1){
		dip <- rep(dip, vectLen)
	}
	if(length(KH) == 1){
		KH <- rep(KH, vectLen)
	}
	if(length(simulations) == 1){
		simulations <- rep(simulations, vectLen)
	}

	# Loop over the parameter vector.
	for(i in 1:vectLen){

		# Simulate for set of parameters.
		res[[i]]<-simPCR(ncells=ncells[i], probEx=probEx[i], probAlq=probAlq[i], probPCR=probPCR[i],
			cyc=cyc[i], tDetect=tDetect[i], dip=dip[i], KH=KH[i],sim=simulations[i])
		
	}

	return(res)

}

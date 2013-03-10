################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 05: Roxygenized.
# 04: New parameters acceptedDev, stepSize, progress
# 03: New parameters seed and maxPCRprob

#' @title Find PCR probabilities
#'
#' @description
#' \code{findPCRprob} Find values for the PCR efficiency parameter for
#'  simPCR that satisfy the target locus balances.
#'
#' @details
#' The interlocus balance for a kit should be characterised during the
#' internal validation of the kit. This function search for PCR efficiency
#' parameters that upon PCR simulation result in similar locus balances.
#' 
#' @param sim integer for number of simulations.
#' @param targetLb numeric vector with target interlocus balances.
#' @param acceptedDev numeric, accepted deviation from target.
#' @param stepSize numeric, probPCR is changed by this value.
#' @param seed numeric, start value for optimisation of probPCR.
#' @param maxPCRprob numeric, maximal value for optimised PCR efficiency.
#' @param progress logical, print progress to console.
#' @param ... arguments to be passed to \code{\link{simPCR}}
#' 
#' @return data.frame with additional columns 'N' and 'Mean'.
#' 
#' @export
#' @examples
#' target <- c(0.05,0.05,0.06,0.04,0.04,
#'              0.04,0.06,0.05,0.05,
#'              0.04,0.04,0.05,0.05,
#'              0.15,0.07,0.09,0.07)
#'              
#' findPCRprob(sim=100, targetLb=target, ncells=200, progress=TRUE)

findPCRprob <- function(sim, targetLb, acceptedDev=0.01, stepSize=0.001, 
			seed=0.75, maxPCRprob=0.95, progress=FALSE, ...){

	# Declare variables.
	simData <- list()
	optimised <- rep(FALSE, length(targetLb))
	lap <- 0
	
	# Start with seeded PCR probability for all loci.
	prob <- rep(seed, length(targetLb))
	prevProb <- rep(0, length(targetLb)) # Initialize previous prob to 0%.

	# Repeat until all PCR probabilities have been optimised.
	while(!all(optimised)){
	
		# Loop over all loci.
		for(i in seq(along=targetLb)){

			# Simulate using current PCR probability.
			simData[i] <- simPCRexplorer(simulations=sim, probPCR=prob[i], ...)

		}

    # Calculate total peak height for each simulation (i.e. per loci):
    # Not needed when simulation is per allele (not per locus)
		#simDataTPH <- addSimAlleles(data=simData)

		# Vectorize result (all in one vector).
		#simDataV <- unlist(simDataTPH)
		simDataV <- unlist(simData)
		
    # Set dimensions (split per loci).
		dim(simDataV) <- c(sim, length(targetLb))

		# Calculate total peak height across all loci per simulation.
		phSum <- rowSums(simDataV)
		
		# Calculate proportion (locus balance) for each simulation.
		simProp <- simDataV / phSum
		
		# Calculate the mean locus balance across all simulations.
		simLb <- colMeans(simProp)

		# Compare the means to the target locus balance.
		diffLb <- simLb - targetLb

		# Check if any simulated mean is within accepted range.
		acceptedLoci <- abs(diffLb) < acceptedDev
		if(any(acceptedLoci)) {
			
			# Set flag for accepted loci.
			optimised[acceptedLoci] <- TRUE
			
		}

		# Optimise as long as not all is accepted.
		if(!all(acceptedLoci)) {

			# Increase counter.
			lap <- lap +1

			# Indicate direction of change for PCR probability.
			sign <- diffLb 
			sign[diffLb <0] <- 1
			sign[diffLb >=0] <- -1
			sign[acceptedLoci] <- 0

			# Print progress.
			if(progress){
				print(paste("Round#",lap))
				print("PCR probability:")
				print(prob)
				print(paste("Simulated inter locus balance (", sim, "simulations):"))
				print(simLb)
				print("Change PCR probability:")
				print(sign)
				# Update console
				flush.console()     
			}

			# Save current unaccepted PCR efficiencies.
			tmpProb <- prob[!acceptedLoci]

			# Change PCR probability in the desired direction.
			prob[!acceptedLoci] <- prob[!acceptedLoci] + sign[!acceptedLoci] * stepSize

			# Decrease all if any PCR probability is more than 1.
			if (any(prob > maxPCRprob)){
				prob <- prob - stepSize
			}
			# Increase all if any PCR probability is less than 0.
			if (any(prob < 0)){
				prob <- prob + stepSize
			}

			# Set saved PCR efficiency as previous PCR efficiency.
			prevProb[!acceptedLoci] <- tmpProb

		}

	}
		
	return(prob)

}

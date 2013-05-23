################################################################################
# TODO LIST
# TODO: Start with a big change, remember the direction of the previous change.
#       If change sign, half the size of the next change. Will be faster... 

################################################################################
# CHANGE LOG
# 19.05.2013: Description of parameters.

#' @title Find scaling factor
#'
#' @description
#' \code{findScaling} Find a peak height scaling value for simPCR that satisfy
#' the target average peak height.
#'
#' @details
#' Experimental function.
#' 
#' @param sim integer for number of simulations.
#' @param targetH numeric vector with target average peak height.
#' @param acceptedDev numeric, accepted deviation from target.
#' @param stepSize numeric, scaling is changed by this value.
#' @param seed numeric, start value for optimisation of scaling.
#' @param maxRounds numeric, maximal number of iterations.
#' @param progress logical, print progress to console.
#' @param tDetect numeric, detection threshold.
#' @param cyc integer, number of PCR cycles.
#' @param ncells integer, number of cells.
#' @param debug logical, print debug information to console.
#' 
#' @return numeric vector with scaling factors.
#' 

findScaling <- function(sim, targetH, acceptedDev=100, stepSize=1,
                        seed=0, maxRounds=10000, progress=FALSE,
                        tDetect=1.25*10^6, cyc=30, ncells=83, debug=FALSE){

  
  probPCR = c(0.99)
 
  # Declare variables.
	simData <- list()
	optimised <- FALSE
	lap <- 0
  debug <- FALSE
  prevDiffH <- 0
  prevDirection <- 0
  prev2Direction <- 0
  dynStepSize <- stepSize
	
	# Start with seeded PCR probability for all loci.
	scaling <- rep(seed, length(targetH))
	prevScaling <- rep(0, length(targetH)) # Initialize previous prob to 0%.

  if(debug){
    print("probPCR")
    print(probPCR)
    print("sim")
    print(sim)
    print("cyc")
    print(cyc)
    print("ncells")
    print(ncells)
    print("tDetect")
    print(tDetect)
    # Update console
    flush.console()     
  }

  # Repeat until all PCR probabilities have been optimised.
	while(!all(optimised)){
	
		# Loop over all loci.
		for(i in seq(along=probPCR)){

			# Simulate using current PCR probability.
			simData[i] <- simPCRexplorer(simulations=sim, KH=scaling, probPCR=probPCR[i], 
                                   cyc=cyc, tDetect=tDetect, ncells=ncells)

		}

		# Vectorize result (all in one vector).
		simDataV <- unlist(simData)
		
    # Set dimensions (split per loci).
		dim(simDataV) <- c(sim, length(probPCR))

		# Calculate average peak height for a sample per simulation.
		avgPh <- rowSums(simDataV) / (2*length(probPCR))
		
		# Calculate the mean peak height across all simulations.
		simH <-mean(avgPh)

		# Compare the means to the target average peak height.
		diffH <- simH - targetH
    
		# Check if any simulated mean is within accepted range.
		accepted <- abs(diffH) < acceptedDev
    
    if(debug){
      print("lap")
      print(lap)
      print("simDataV")
      print(simDataV)
      print("simH")
      print(simH)
    }
    
		if(any(accepted)) {
			
			# Set flag for accepted loci.
			optimised[accepted] <- TRUE
			
		}

		# Optimise as long as not all is accepted.
		if(!all(accepted)) {

			# Increase counter.
			lap <- lap +1

			# Indicate direction of change for scaling.
			sign <- diffH 
			sign[diffH <0] <- 1
			sign[diffH >=0] <- -1
			sign[accepted] <- 0

			# Print progress.
			if(progress){
				print(paste("Round#",lap))
				print("Scaling:")
				print(scaling)
				print(paste("Simulated average peak height (", sim, "simulations):"))
				print(simH)
				print("Change scaling:")
				print(sign)
				# Update console
				flush.console()     
			}

			# Save current unaccepted PCR efficiencies.
			#tmpProb <- prob[!accepted]

			# Change scaling in the desired direction.
      dScaling <- abs((diffH-prevDiffH)/diffH)
      

# 			if(sign == prevDirection & prevDirection == prev2Direction){
#         print("S=P=P2")
#         dynStepSize <- dynStepSize * 2
# 			  scaling <- scaling + sign * dynStepSize
# 			} else if(sign != prevDirection & prevDirection == prev2Direction){
# 			  print("S!=P==P2")
# 			  dynStepSize <- dynStepSize * 0.5
# 			  scaling <- scaling + sign * dynStepSize
# 			} else{
# 			  print("ELSE")
#         
# 			        if(dScaling < 0.05){
# 			          boost <- 100
# 			        } else if(dScaling < 0.5){
# 			          boost <- 10
# 			        } else {
# 			          boost <- 1
# 			        }
# 			        
# 			    		scaling <- scaling + sign * stepSize * boost
# 			}
			
      
			scaling <- scaling + sign * stepSize
			
      prevDiffH <- diffH
			#prev2Direction <- prevDirection
			#prevDirection <- sign

      
      # Check max.
			if (lap > maxRounds){
				return("Unsuccessful +")
			}
      
			# Check 0.
			if (any(scaling < 0)){
			  return("Unsuccessful -")
			}

		}

	}
		
			# Print progress.
			if(progress){
				print(paste("Round#",lap))
				print("Scaling:")
				print(scaling)
				print(paste("Simulated average peak height (", sim, "simulations):"))
				print(simH)
				print("Change scaling:")
				print(sign)
				# Update console
				flush.console()     
			}

	return(scaling)

}

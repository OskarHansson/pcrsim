################################################################################
# TODO LIST
# TODO: remove vectorization (not possible in the fixed loop '13') Add stutters.
# TODO: Validate and calibrate the model.
# TODO: Clean up code.
# TODO: Implement CE parameters?

# Remember: x*y = exp(log(x)+log(y))

################################################################################
# CHANGE LOG
# 07.05.2013: Formula for calculating rfu changed
#             from: log((tmpA+tDetect)/tDetect) * KH
#             to: (tmpA/tDetect) * KH
# <07.05.2013: Fixed compatibility issue with R>=3 (rbinom returns integer -> NA for large numbers)
# <07.05.2013: Added more debugging and verifications.
# <07.05.2013: Roxygenized.
# <07.05.2013: Modelling degradation by giving ncells(quant) per allele.
#              Does not work for sperm cells...


#' @title PCR simulator
#'
#' @description
#' \code{simPCR} simulates the PCR process.
#'
#' @details
#' Based on \code{simPCR2} from the \code{forensim} package by Hinda Haned.
#' Simulates PCR for a single allele by a series of binomial distributions.
#' 
#' @param ncells integer for number of cells/dna molecules for the current allele.
#' Can be vector of integers giving the number of cells in each simulation.
#' @param probEx numeric for probability that an allele survives the extraction
#'  (extraction efficiency).
#' Can be vector of numeric giving the probability in each simulation.
#' @param probAlq numeric, probability that an allele is aliquoted into the PCR reaction.
#' Can be vector of numeric giving the probability in each simulation.
#' @param probPCR numeric, probability that an allele is amplified during a PCR cycle (PCR efficiency).
#' Can be vector of numeric giving the probability in each simulation.
#' @param cyc integer, number of PCR cycles.
#' @param tDetect integer, detection threshold. Number of molecules needed to trigger a signal.
#' @param KH integer, correlation factor for number of molecules to peak height.
#' @param sim integer, number of simulations.
#' @param dip logical flagging for diploid cells (haploid cells are currently not implemented)
#' 
#' @return list with simulation results.
#' @export
#' @examples
#' simPCR(ncells=100, probEx=0.7, probAlq=0.1, probPCR=0.85, sim=100)


simPCR<-function(ncells, probEx=1, probAlq=1, probPCR=1, cyc=28, 
		tDetect=5*10^7, dip=TRUE, KH=55, sim=1) {

  # Constants.
  debug=FALSE
  imax <- .Machine$integer.max
  
	# Debug info.
	if(debug){
	  print(paste("IN:", match.call()[[1]]))
		flush.console()
	}


  if(dip){
    
  	# CONSTANTS
  	# - probability of one or the other allele in haploid cells (i.e. sperm cells).
  	# pHaploid=0.5
  
  	# Debug info.
  	if(debug){
  	  print("ncells:")
  	  print(head(ncells))
  	  print("probEx:")
  	  print(head(probEx))
  	  print("probAlq:")
  	  print(head(probAlq))
  	  print("probPCR:")
  	  print(head(probPCR))
  		print(paste("cyc:", cyc))
  		print(paste("tDetect:", tDetect))
  		print(paste("dip:", dip))
  		print(paste("KH:", KH))
  		print(paste("sim:", sim))
  		flush.console()
  	}
  
  	# STANDARD VERIFICATIONS
  	
  	if(is.null(ncells) || !is.numeric(ncells) || ncells <0){
  		stop("'ncells' must be a numeric giving the number of cells")
  	}

  	if(!is.numeric(cyc) || is.na(cyc) || cyc < 1){
  	    stop("Th number of PCR cycles must at least equal 1")
  	}

  	if(!is.numeric(tDetect) || is.na(tDetect) || tDetect < 0){
  	  stop("'tDetect' must be a positive numeric")
  	}
    
    #cheking the probabilities input parameters:
  	#probEx: extraction efficiency
  	if(!is.numeric(probEx) || is.na(probEx) || probEx <0 || probEx >1){
          stop("'probEx' is a probability, it must belong to [0,1]")
    }
  
  	#probAlq: probability of surviving for aliquots
  	if(!is.numeric(probAlq) || is.na(probAlq) || probAlq <0 || probAlq >1){
          stop("'probAlq' is a probability, it must belong to [0,1]")
    }
  	
  	#probPCR: PCR efficiency
  	if(!is.numeric(probPCR) || is.na(probPCR) || probPCR <0 || probPCR >1){
          stop("'pprobPCR' is a probability, it must belong to [0,1]")
    }
  	
  	#cyc: PCR cycle
  	if(!is.numeric(cyc) || is.na(cyc) || cyc <=0){
  		stop("'cyc' is the number of PCR cycles, it must be an integer > 0")
  	}
  	
  	#At this point, we have all the input parameters

  	# Calculate the number of DNA molecules for the allele.
  	ndna <- floor(ncells)
  	
  	
  	##################1st EXTRACTION STEP
  
		# Number of alleles surviving the extraction process: nAs, are generated from a binomial distribution
		# with parameters ndna (number of dna molecules) and Probex (extraction efficiency)

  	nAs<- rbinom(n=sim,size=ndna,prob=probEx)

  	# Debug info.
  	if(debug){
  	  print("nAs")
  	  print(nAs)
  	  print(class(nAs))
  	  flush.console()
  	}
  	
  	##################2nd EXTRACTION STEP: aliquots
  
  	# Aliquots of type A
  	nA<-rbinom(n=sim,size=nAs,prob=probAlq)

  	# Debug info.
  	if(debug){
  	  print("nA")  
  	  print(nA)
  	  print(class(nA))
  	  flush.console()
  	}
  	
  	##################PCR efficiency: 
  
  	# For each cycle (defind in cyc)
  	tmpA<-as.numeric(nA)
    
    for(c in 1:cyc) {

      # NB! To avoid NA (integer overflow) in rbinom (R >= 3.0.0):
      # Divide in max integer and add up after rbinom.
      iChunks <- floor(tmpA / imax)
      rest <- tmpA - iChunks * imax 
      # Debug info.
      if(debug){
        print("iChunks (number of imax chunks)")  
        print(iChunks)
        print("tmpA")  
        print(tmpA)
        print("rest")  
        print(rest)
        print("if(iChunks>0)")
        print(iChunks>0)
        flush.console()
      }
      
      for(s in 1:sim){
        
        if(iChunks[s] > 0) {

          # Run rbinom for all 
          for(b in 1:iChunks[s]){
            tmpA[s] <- tmpA[s] + as.numeric(rbinom(n=1, size=imax, prob=probPCR))
          }
          # Add the rest.
          tmpA[s] <- tmpA[s] + as.numeric(rbinom(n=1, size=rest[s], prob=probPCR))
        } else {
          tmpA[s] <- tmpA[s] + as.numeric(rbinom(n=1, size=rest[s], prob=probPCR))
        }
      }
      
#       # ORIGINAL (DOES NOT WORK FOR VECTORS)
#       if(i>0) {
#         for(b in 1:i){
#           tmpA <- tmpA + as.numeric(rbinom(n=sim, size=imax, prob=probPCR))
#         }
#       }
#       tmpA <- tmpA + rbinom(n=sim, size=rest, prob=probPCR)
      
  	}
  
  	###################CE parameters:
  	#TODO: aliquots of type A 1 ?L of 25 ?L / 50 ?L --> p=0,04 and p=0,02
  	#TODO: probability fragment injected into capillary

    #detection threshold T=2x10^7
  	#converting from number of molecules to peak heights: to be improved
  	#diploid case
  
  	#generating peak heights: this might be subject to change during model calibration
  	# +tDetect to avoid 0 and negative values.
  	
  	# Debug info.
  	if(debug){
  	  print("tmpA")    
  	  print(tmpA)    
  	  flush.console()
  	}
    
  	vecH1 <- (tmpA/tDetect) * KH
  	res <- round(vecH1)

    if(any(is.na(res))){
      warning(paste("Simulation contains NA", match.call()[[1]]))
    }
    
  	# Debug info.
  	if(debug){
  		print("res:")
  		print(head(res))
  		flush.console()
  	}
  
  
  	# Debug info.
  	if(debug){
  	  print(paste("EXIT:", match.call()[[1]]))
  		flush.console()
  	}
  
  	# Return simulated peak heights.
  	return(res)
  } else if (!dip){
    
  } else {
    
    stop("dip must be a logical indicating 'diploid' (TRUE) or 'haploid' (FALSE) cells!")
    
  }
	
}

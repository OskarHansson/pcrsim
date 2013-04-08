################################################################################
# TODO LIST
# TODO: Check that 'alleles' contain required columns etc...
# TODO: Check if homozygotes are modelled correct (2*amount).
################################################################################
# NB! [if ilb is applied post-PCR] :
# When calculating interlocus balance the locus with highests peaks should be set to 1
# (because this will have the highest PCR efficiency, and thus prevent overestimating the peak heights)
# and the rest will be less than 1.

################################################################################
# CHANGE LOG
# 17: Prevent valus >1 x[x>1] <- 1 for probabilities.
# 16: Distributions for degradation is working.
# 15: Changed control: either ncells, conc+vol OR slop+interc+vol.
# 14: Prevent negative valus x[x<0] <- 0 for some distributions/probabilities.
# 13: Implemented distributions for parameters.
# 13: Using df and slim format.
# 13: Conversion of various df input for 'alleles'.
# 12: Roxygenized.
# 11: Output a data frame instead of a list.
# 10: Implementation of degradation parameters.
# 10: Added parameter 'amount' to use instead of passing 'ncells'
# 09: New try 2013.

#' @title Simulate DNA profiles
#'
#' @description
#' \code{simulateProfile} simulates entire DNA profiles.
#'
#' @details
#' Simulates the peak heights for a single person profile.
#' Integration of degradation and interlocus balance.
#' Handle missing alleles.
#' Only single person profiles, unique(alleles) will be applied.
#' 
#' @param alleles list of alleles per marker.
#' @param ncells numeric, number of cells ( = number of DNA molecules of each allele).
#' @param ncells.sd numeric, standard deviation.
#' @param conc numeric, human DNA concentration in DNA extract.
#' @param conc.sd numeric, standard deviation.
#' @param intercept numeric, degradation intercept.
#' @param intercept.sd numeric, standard deviation for degradation intercept.
#' @param slope numeric, degradation slope.
#' @param slope.sd numeric, standard deviation for degradation slope.
#' @param exprob numeric [0-1], probability that an allele survivives the extraction
#'  (extraction efficiency).
#' @param exprob.sd numeric, standard deviation for the extraction efficiency.
#' @param volume, numeric, final extraction volume.
#' @param volume.sd, numeric, standard deviation for the final extraction volume.
#' @param aliq numeric, volume of DNA extract taken for PCR amplification.
#' @param aliq.sd numeric, standard deviation for volume of DNA extract.
#' @param kit string or integer specifying an STR typing kit.
#' @param simulations integer, number of simulations.
#' @param celldna, numeric giving the DNA content of a cell.
#' @param ... arguments to be passed to \code{\link{simPCR}}
#' 
#' @return list with simulation results.
#' @export
#' @examples
#' # Define a DNA profile.
#' dnaProfile <- list(c("X","Y"),c(17,18),c(6,9.3),c(29,31.2),c(16,18),
#' c(13,15),c(12,13),c(22,25),c(9,13),
#' c(16,16),c(16,19),c(14,15),c(20,23),
#' c(10,14),c(18,23),c(13,14),c(15,16))
#' 
#' # Simulate.
#' simulateProfile(alleles=dnaProfile, kit="ESX17", simulations=5,
#'  ncells=200, ncells.sd=20, 
#'  exprob=0.7, exprob.sd=0.1,
#'  volume=200, volume.sd=20,
#'  aliq=17.5, aliq.sd=1)

simulateProfile<-function(alleles,
                          ncells=NA, ncells.sd=0,
                          conc=NA, conc.sd=0,
                          intercept=NA, intercept.sd=0, 
                          slope=NA, slope.sd=0, 
                          exprob=NA, exprob.sd=0,
                          volume=NA, volume.sd=0,
                          aliq=NA, aliq.sd=0,
                          simulations=1, kit=NA,
                          celldna=0.006, ...){

  # Constants.
  debug=FALSE
  
  # Load dependencies.
  require(strvalidator)
  
  # Debug info.
  if(debug){
    print(paste("IN:", match.call()[[1]]))
    print("Kit:")
    print(kit)
    print("ncells:")
    print(ncells)
    print("volume:")
    print(volume)
    print("alleles:")
    print(alleles)
    flush.console()
  }

  # Set flags and initiate constants.
  degraded <- FALSE    # Flag for degraded DNA.
  rvolume <- NULL      # Extraction volumes.
  dna <- NULL          # Proxy for number of DNA molecules.
  rintercept <- NULL
  rslope <- NULL
  allelesDf <- NULL

  # Pre-allocate result data frame.
  # Create an empty data frame 3 times the length of 'data'.
  resultDf <- data.frame(matrix(NA, 1, 3))
  names(resultDf) <- c("Marker", "Allele", "Height")
  # Remove all NAs
  resultDf <- resultDf[-1,]
  
  # PARAMETER CONTROL AND #####################################################
  # GENERATION OF PARAMETER SET ###############################################

  # KIT -----------------------------------------------------------------------
  
  # Get kit information.
  if(!is.null(kit) && !is.na(kit)){
    kitInfo <- getKit(kit)
    interLocusBalance <- kitInfo$probPCR
    markers <- kitInfo$locus
  } else{
    stop("'kit' must be provided!")
  }
  
  # EXTRACTION VOLUME ---------------------------------------------------------
  
  if(!is.null(volume) && !is.na(volume)){
    
    # Draw random extraction volumes for each simulation.
    rvolume <- rnorm(simulations, volume, volume.sd)
    rvolume[rvolume < 0] <- 0
    
  } else{
    stop("Extraction volume must be provided!")
  }

  # NUMBER OF DNA MOLECULES ---------------------------------------------------

  if(!is.null(ncells) && !is.na(ncells)){
    
    # Draw the number of cells for each simulation.
    dna <- rnorm(simulations, ncells, ncells.sd)
    dna[dna < 0] <- 0
    
    # Debug info.
    if(debug){
      print("dna drawn from ncells")
      print("dna:")
      print(dna)
      flush.console()
    }

  # Estimate from concentration.
  } else if ((!is.null(conc)  && !is.na(conc)) &
         (!is.null(rvolume) && !is.na(rvolume))){
      
    # Draw random concentrations for each simulation.
    rconc <- rnorm(simulations, conc, conc.sd)
    rconc[rconc < 0] <- 0
    
    # Approximate the number of cells for each simulation.
    dna <- (rconc * rvolume) / celldna

    # Debug info.
    if(debug){
      print("dna estimated from concentration")
      print("dna:")
      print(dna)
      flush.console()
    }

  # Estimate from concentration.
  } else if ((!is.null(intercept)  && !is.na(intercept)) &
     (!is.null(slope) && !is.na(slope))){
    # Set flag.
    degraded <- TRUE
  } else{
      
      stop(paste("Either an estimate of the number of cells",
           "OR the concentration and extraction volume",
           "OR the slope, intercept and extraction volume must be provided!"))
  
  }
    
  # EXTRACTION PROBABILITY ----------------------------------------------------
  
  if(!is.null(exprob) && !is.na(exprob)){

    # Draw random extraction probabilities for each simulation.
    rexprob <- rnorm(simulations, exprob, exprob.sd)
    rexprob[rexprob < 0] <- 0
    rexprob[rexprob > 1] <- 1
    
  } else{
    stop("Extraction probability must be provided!")
  }
  
  # ALIQUOTE PROBABILITY ------------------------------------------------------
  
  if((!is.null(aliq)  && !is.na(aliq)) &
       (!is.null(rvolume) && !is.na(rvolume))){

    # Draw random aliguotes for each simulation.
    raliq <- rnorm(simulations, aliq, aliq.sd)
    raliq[raliq < 0] <- 0
    
    # Calculate 'probAlq'.
    paliq <- raliq / rvolume
    paliq[paliq < 0] <- 0
    paliq[paliq > 1] <- 1
    
  } else{
    stop("Extraction volume and an aliquote must be provided!")
  }

  # ALLELES -------------------------------------------------------------------
  
  # Check data format.
	if(is.list(alleles) && !is.data.frame(alleles)){

    # Convert to data frame.
	  allelesDf <- listToDataframe(data=alleles, kit=kit, colName="Allele")

    # Debug info.
	  if(debug){
	    print("alleles is list - convert to data.frame:")
	    print(allelesDf)
	    flush.console()
	  }
    
	} else if (is.data.frame(alleles)){

    allelesDf <- alleles

    # Debug info.
	  if(debug){
	    print("alleles is data.frame:")
	    print(allelesDf)
	    flush.console()
	  }

	} else {
    stop("'alleles' must be provided as a list or data frame!")
	}

  # Check if 'fat' format.
  if(length(grep("Allele", names(allelesDf))) > 1) {

    # Debug info.
    if(debug){
      print("alleles is 'fat' format:")
      print(allelesDf)
      flush.console()
    }
    
	  # Slim data frame.
    allelesDf <- slim(data=allelesDf, fix=c("Marker"), stack=c("Allele"))

    # Debug info.
    if(debug){
      print("convert to 'slim' format:")
      print(allelesDf)
      flush.console()
    }

  }

  # DEGRADATION ---------------------------------------------------
  
	if(degraded){

    # Draw random slope and intercept for each simulation.
    rintercept <- rnorm(simulations, intercept, intercept.sd)
    rslope <- rnorm(simulations, slope, slope.sd)
    
    # Get size in base pair.
		allelesDf <- alleleToSize(data=allelesDf, kit=kit)
    
	  # Debug info.
	  if(debug){
	    print("'Size' added:")
	    print(allelesDf)
			flush.console()
		}
	}

  # DEBUG ---------------------------------------------------------------------
  
  # Debug info.
  if(debug){
    print("aliquote:")
    print(head(raliq))
    print("probAlq:")
    print(head(paliq))
    print("rintercept:")
    print(head(rintercept))
    print("slope:")
    print(head(rslope))
    print("alleles:")
    print(alleles)
    print("markers:")
    print(markers)
    print("probPCR:")
    print(head(interLocusBalance))
    print("dna:")
    print(head(dna))
    print("conc:")
    print(head(conc))
    print("rvolume:")
    print(head(rvolume))
    flush.console()
  }
  
  # SIMULATE ##################################################################

  # MARKER LOOP ---------------------------------------------------------------

	# Loop over all loci.
	for (locus in seq(along=markers)) {

    # Get current marker name.
    cMarker <- markers[[locus]]
    
    # Get alleles for current marker.
    cAlleles <- allelesDf$Allele[allelesDf$Marker==cMarker]
    
    # Replace string "NA" with NA.
    cAlleles[cAlleles=="NA"] <- NA
    cAlleles <- unique(cAlleles)

    # Check if degraded and adjust number of DNA molecules per allele.
    if(degraded){
      
      # Get allele sizes for current marker.
      cAsize <- allelesDf$Size[allelesDf$Marker==cMarker]

      # Debug info.
      if(debug){
        print("cAsize:")
        print(cAsize)
        flush.console()
      }
    }

    # Get information required 
    cLb <- interLocusBalance[locus]

    # ZYGOSITY ----------------------------------------------------------------
    
    # Determine if heterozygote or homozygote.
    nbAlleles <- length(cAlleles)  # Get number of alleles (should always be 2).
    anyNA <- any(is.na(cAlleles))  # Check for NA's.
    allNA <- all(is.na(cAlleles))  # Check if only NA's.

    # Debug info.
    if(debug){
      print("nbAlleles:")
      print(nbAlleles)
      print("anyNA:")
      print(anyNA)
      print("allNA:")
      print(allNA)
      flush.console()
    }
    
    if (nbAlleles == 2 && anyNA == FALSE) {
      heterozygote <- TRUE
    } else if (nbAlleles == 1 || anyNA == TRUE) {
      heterozygote <- FALSE
    } else {
      stop("ERROR in setting heterozygous flag!")
    }

    # Debug info.
    if(debug){
      print("cMarker:")
      print(cMarker)
      print("cAlleles:")
      print(cAlleles)
      print("cLb:")
      print(cLb)
      flush.console()
    }
    
    # ALLELE LOOP -------------------------------------------------------------
    
    # Loop over all alleles.
	  for (a in seq(along=cAlleles)) {
	    
      # Get current allele.
  		cA <- cAlleles[a]

      # Double quant for homozygotes.
      if(heterozygote){
        if(degraded){
          # Estimate amount (concentration*volume) of DNA for each simulation.
          # y=10^mx+k   [y=concentration, x=size, k=intercept]
          cQuant <- 10^(rslope * cAsize[a] + rintercept) * rvolume
        } else {
          cQuant <- dna
        }
      } else {
        if(degraded){
          cQuant <- (10^(rslope * cAsize[a] + rintercept) * rvolume) * 2
        } else {
          cQuant <- dna * 2
        }
      }
  
  		# Debug info.
  		if(debug){
  		  print("cA:")
  		  print(cA)
  		  print("cQuant:")
  		  print(head(cQuant))
  		  flush.console()
  		}
  		
  		# Check if allele.
  		if(!is.na(cA)){
  
  			# Debug info.
  			if(debug){
  				print(paste("probPCR:", cLb))
  				print(paste("simulations:", simulations))
  				print("... :")
  				print(list(...))
  				flush.console()
  			}
  
  			# Simulate pcr for an allele , the specified number of times.
  			tmpPH <- simPCR(ncells=cQuant, probEx=rexprob, probAlq=paliq, probPCR=cLb, 
                         sim=simulations, ...)
  
        
  			# Debug info.
  			if(debug){
  				print("tmpPH:")
  				print(head(tmpPH))
  				flush.console()
  			}
  
  
  		} else { # There were no alleles.
  			tmpPH <- rep(NA, simulations)
  		}

  		tmpData <- data.frame("Marker"=cMarker, 
  		                      "Allele"=cA, 
  		                      "Height"=tmpPH,
  		                      stringsAsFactors=FALSE)
  		
      
  		# Bind data.
  		resultDf <- rbind(resultDf, tmpData) 
  
  	}
    
	}

	# Debug info.
	if(debug){
		print("resultDf:")
		print(head(resultDf))
		flush.console()
	}

	# Debug info.
	if(debug){
	  print(paste("EXIT:", match.call()[[1]]))
		flush.console()
	}

	# Return a data frame with peak heights for the simulated profile.
	return(resultDf)

}

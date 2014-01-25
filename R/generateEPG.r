################################################################################
# TODO LIST
# TODO: Don't calculate Size if present in data.
# TODO: Can't handle no peaks in a dye channel.
# NB! Missing alleles should be empty string ""
# (then they will not be visible label and not generate error in ggplot2)

################################################################################
# CHANGE LOG
# 25.01.2014: Updated for compatibility with strvalidator 1.0.0.
# <25.01.2014: Removed 10% margin because of aes_string
# <25.01.2014: Conversion from 'fat' to 'slim' format.
# <25.01.2014: Roxygenized.
# <25.01.2014: Automatic detects distributions.
# <25.01.2014: Distributions.
# <25.01.2014: Marker ranges and allele names.
# <25.01.2014: First version

#' @title Generate EPG
#'
#' @description
#' \code{generateEPG} visualises an EPG from DNA profiling data.
#'
#' @details
#' Generates a electropherogram like plot from 'data' and 'kit'.
#' Homozygotes should be entered as a single entry.
#' 
#' @param data data frame containing columns 'Allele', 'Marker' and 'Height'.
#' @param kit string or integer representing the STR typing kit.
#' @param plotTitle string providing the title for the EPG.
#' @param peaks logical, TRUE plot peaks using mean peak height for distributions.
#' @param debug logical for printing debug information to the console.
#' 
#' @return NULL.
#' 
#' @keywords internal
#' @export
#' @examples
#' # Load package.
#' library(pcrsim)
#' library(strvalidator)
#' # Load genotyping data.
#' data("set1")
#' # Extract a sample.
#' mySample <- set1[set1$Sample.Name=="PC1",]
#' # Generate an electropherogram like plot.
#' generateEPG(data=mySample, kit="ESX17")


generateEPG <- function(data, kit, plotTitle=NULL, peaks=TRUE, debug=FALSE){

  # Debug info.
	if(debug){
	  print(paste("IN:", match.call()[[1]]))
		print("data:")
		print(data)
		print("kit:")
		print(kit)
		print("plotTitle:")
		print(plotTitle)
		flush.console()
	} else {}

  # Convert NA to empty string to prevent ggplot2 error (and prevent allele labels)
  data[is.na(data)] <- ""

  # Debug info.
  if(debug){
    print("data2:")
    print(data)
  }
  
	# Constants. # TODO: Better approach to determine if sim# > 1.
	if(nrow(data[duplicated(data[!data$Allele=="",], MARGINS=c("Marker","Allele")),]) > 0){
		distribution=TRUE
	} else {
		distribution=FALSE
	}
	mSpace <- 1.1  # Factor to resize plot area.
	
	# Get kit information.
	kitInfo <- getParameter(kit)

	# Check if 'fat' format.
	if(length(grep("Allele", names(data))) > 1) {
	  
	  # Debug info.
	  if(debug){
	    print("alleles is 'fat' format:")
	    print(data)
	    flush.console()
	  } else {}
	  
	  # Slim data frame.
	  data <- slim(data=data, fix=c("Marker"), stack=c("Allele","Height"), debug=debug)
	  
	  # Debug info.
	  if(debug){
	    print("convert to 'slim' format:")
	    print(data)
	    flush.console()
	  } else {}
	  
	}
  
	# Add unique 'Id' column (combine 'Allele' and 'Marker') for grouping.
	data$Id <- paste(data$Allele, data$Marker, sep="") 

  # Debug info.
  if(debug){
    print("data3:")
    print(data)
  }
  
	# Calculate size of alleles.
	data <- alleleToSize(data=data, kit=kit)

	# Check if distributions.
	if(!distribution){

		# Calculate coordinates for plotting peaks.
		data <- heightToPeak(data=data)

		# Height must be converted from character to numeric.
		# Or there will be problems mixing continous and categorical data on the same axis... 
		data$Height <- as.numeric(data$Height)

	} else {

		# Creat a data table copy of the data frame.
		tmpDT <- data.table(data , keep.rownames=TRUE)

		# Calculate the mean height for each allele.
		tmpDTmph <- tmpDT[, mean(Height), by=Id]

		tmpDFmph <- data.frame(tmpDTmph)
		names(tmpDFmph)<- c("Id","Height")

		tmpX<-unique(data[ , c("Marker","Allele")])
		tmpDF <- cbind(tmpX, tmpDFmph)

		# Calculate size of alleles.
		dataMean <- alleleToSize(data=tmpDF, kit=kit)

		# Calculate coordinates for plotting peaks.
		dataMean <- heightToPeak(data=dataMean)

		# Add unique 'Id' column (combine 'Allele' and 'Marker') for grouping.
		#data$Id <- paste(data$Allele, data$Marker, sep="") 

	}


	# Copy unique 'Marker'-'Allele' combinations in 'data'
	# to a new data frame for handling allele names.
	alleleInfo <- unique(data[ , c("Marker","Allele")])
	# Add dye information.
  # NB! addDye --> addColor
  alleleInfo <- addColor(data=alleleInfo, kit=kit, need="Dye")
	# Calculate size of alleles.
	alleleInfo <- alleleToSize(data=alleleInfo, kit=kit)
  # Replace NA with the smallest size in kit (plot can't handle all NAs).
  alleleInfo$Size[is.na(alleleInfo$Size)] <- min(kitInfo$rangeMin)

	# Add dye information.
	data <- addColor(data=data, kit=kit)
	if(distribution){
		dataMean <- addColor(data=dataMean, kit=kit)
	}

	# Add color information.
  # NB! dyeToColor --> use addColor
  #data <- dyeToColor(data=data)
  if(distribution){
		dataMean <- dyeToColor(data=dataMean)
	}

	# Get colors.
	manualColors <- unique(data$R.Color)	
# NB! sortMarkers --> sortMarker
	# Sort 'Marker' and 'Dye' factors according 'kit'.
	data <- sortMarker(data=data, kit=kit)
	if(distribution){
		dataMean <- sortMarker(data=dataMean, kit=kit)
	}

	# Get information for annotation of markers.
	mDye <- kitInfo$dye
	mXmin <- kitInfo$rangeMin
	mXmax <- kitInfo$rangeMax
	myText <- kitInfo$locus
	mYmax <- vector()

	# Loop over all dye channels.
	for(ci in unique(data$Dye)){
		# Find the maximum value and repeat for the whole current dye channel.
		tmpHeight <- data$Height[data$Dye==ci]
		tmpHeight[is.na(tmpHeight)] <- 1 # Make sure we not end up with NA.
		tmpYmax <- max(tmpHeight)
    if(tmpYmax==0){tmpYmax <- 1} # Make Y max at least 1.
		mYmax <- c(mYmax, 
				rep(tmpYmax, length(unique(data$Marker[data$Dye==ci]))))
	}

  # Make sure numeric.
  mYmax <- as.numeric(mYmax)
  
	# Debug info.
	if(debug){
	  print("data:")
	  print(data)
	  print(str(data))
	  print("mDye:")
	  print(mDye)
	  print("mXmin:")
	  print(mXmin)
	  print("mXmax:")
	  print(mXmax)
	  print("mYmax:")
	  print(mYmax)
	  print("myText:")
	  print(myText)
	  print("mSpace:")
	  print(mSpace)
	  flush.console()
	} else {}  
  
	# Create annotation data frame for loci.
	markerRanges <- data.frame(Dye=factor(mDye),	# Facet.
				Color=mDye,			          # Dye.
				Xmin=mXmin,			          # Marker lower range.	
				Xmax=mXmax,			          # Marker upper range.
				Size=(mXmin+mXmax)/2,		  # Midpoint of marker range.
				Height=mYmax * mSpace,		# Lower edge of marker range.
	      mSpace=mSpace,	          # Pass mSpace to aes_string.
	      Text=myText)			        # Marker names.

	# Debug info.
	if(debug){
		print("markerRanges:")
		print(markerRanges)
		print(str(markerRanges))
		print("alleleInfo:")
		print(alleleInfo)
		print(str(alleleInfo))
		flush.console()
	} else {}

	# Create plot.
	myPlot<- ggplot(data=data, aes_string(x="Size", y="Height"))

	# Add marker regions.
	myPlot <- myPlot + geom_rect(aes(xmin=Xmin, xmax=Xmax,
          ymin=Height, ymax=Height * mSpace),
					alpha = .2, data=markerRanges, fill="blue", color="red")

	# Add marker names.
	myPlot <- myPlot + geom_text(aes(label=Text, y=Height * mSpace), 
						data=markerRanges, size=3, vjust = 1)

	# Plot data.
	if(!distribution){
		# Plot peak height as peaks.
		myPlot <- myPlot + geom_polygon(aes_string(group="Id", fill="Dye"), data=data) + 
						scale_fill_manual(values=manualColors)
	} else {
		# Plot boxplots for distributions.
		myPlot <- myPlot + geom_boxplot(aes_string(group="Id", color="Dye"), 
                        outlier.size=1, data=data) + 
						            scale_colour_manual(values=manualColors)

		if(peaks){
			# Plot mean peak height as peaks.
			myPlot <- myPlot + geom_polygon(aes_string(group="Id", fill="Dye"), 
                          data=dataMean) + 
						              scale_fill_manual(values=manualColors)
		}
	}

	# Add allele names.
	myPlot <- myPlot + geom_text(aes_string(label="Allele", x="Size", y=-Inf), 
						data=alleleInfo, size=3, vjust=0)

	# Facet according to dye channel.
	myPlot <- myPlot + facet_grid(Dye ~ Marker) + 
						facet_wrap(~ Dye, ncol=1, drop=FALSE, scales="free")

	# Strip facet labels.
	myPlot <- myPlot + theme(strip.text = element_blank())

	# Strip facet background.
	myPlot <- myPlot + theme(strip.background = element_blank())

	# Add title and axis labels.
	myPlot <- myPlot + labs(title=plotTitle)
	myPlot <- myPlot + xlab("Size (bp)")
	myPlot <- myPlot + ylab("Peak height (RFU)")

	# Show plot.
	print(myPlot)

	# Debug info.
	if(debug){
	  print(paste("EXIT:", match.call()[[1]]))
		flush.console()
	} else {}

	return(myPlot)

}

################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 07: Removed 10% margin because of aes_string
# 06: Conversion from 'fat' to 'slim' format.
# 05: Roxygenized.
# 04: Automatic detects distributions.
# 03: Distributions.
# 02: Marker ranges and allele names.
# 01: First version

#' @title Generate EPG
#'
#' @description
#' \code{generateEPG} visualises an EPG from DNA profiling data.
#'
#' @details
#' ggplot2 version of plotEPG. Require packages 'ggplot2' and 'data.table'.
#' Generates a electropherogram like plot from 'data' and 'kit'.
#' 
#' @param data data frame containing columns 'Allele', 'Marker' and 'Height'.
#' @param kit string or integer representing the STR typing kit.
#' @param plotTitle string providing the title for the EPG.
#' @param peaks logical, TRUE plot peaks using mean peak height for distributions.
#' @param debugInfo logical for printing debug information to the console.
#' 
#' @return NULL.
#' 
#' @keywords internal
#' @export
#' @examples
#' data("set1")
#' mySample <- set1[set1$Sample.Name=="PC1",]
#' generateEPG(data=mySample, kit="ESX17")


generateEPG <- function(data, kit, plotTitle=NULL, peaks=TRUE, debugInfo=FALSE){

  require(ggplot2)
  require(data.table)
  require(strvalidator)
  
  # Debug info.
	if(debugInfo){
		print("ENTER: generateEPG")
		print("data:")
		print(data)
		print("kit:")
		print(kit)
		print("plotTitle:")
		print(plotTitle)
		flush.console()
	} else {}

	# Constants.
	if(nrow(data[duplicated(data, MARGINS=c(1,2)),]) > 0){
		distribution=TRUE
	} else {
		distribution=FALSE
	}
	markerSpace <- 1.1  # Factor to resize plot area.
	
	# Get kit information.
	kitInfo <- getKit(kit)

	# Check if 'fat' format.
	if(length(grep("Allele", names(data))) > 1) {
	  
	  # Debug info.
	  if(debugInfo){
	    print("alleles is 'fat' format:")
	    print(data)
	    flush.console()
	  } else {}
	  
	  # Slim data frame.
	  data <- slim(data=data, fix=c("Marker"), stack=c("Allele","Height"))
	  
	  # Debug info.
	  if(debugInfo){
	    print("convert to 'slim' format:")
	    print(data)
	    flush.console()
	  } else {}
	  
	}
  
	# Add unique 'Id' column (combine 'Allele' and 'Marker') for grouping.
	data$Id <- paste(data$Allele, data$Marker, sep="") 


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
	alleleInfo <- addDye(data=alleleInfo, kit=kit)
	# Calculate size of alleles.
	alleleInfo <- alleleToSize(data=alleleInfo, kit=kit)

	# Add dye information.
	data <- addDye(data=data, kit=kit)
	if(distribution){
		dataMean <- addDye(data=dataMean, kit=kit)
	}

	# Add color information.
	data <- dyeToColor(data=data)
	if(distribution){
		dataMean <- dyeToColor(data=dataMean)
	}

	# Get colors.
	manualColors <- unique(data$Color)	

	# Sort 'Marker' and 'Dye' factors according 'kit'.
	data <- sortMarkers(data=data, kit=kit)
	if(distribution){
		dataMean <- sortMarkers(data=dataMean, kit=kit)
	}

	# Get information for annotation of markers.
	myDye <- kitInfo$dye
	myXmin <- kitInfo$rangeMin
	myXmax <- kitInfo$rangeMax
	myText <- kitInfo$locus
	myYval <- vector()

	# Loop over all dye channels.
	for(ci in unique(data$Dye)){
		# Find the maximum value and repeat for the whole current dye channel.
		tmpHeight <- data$Height[data$Dye==ci]
		tmpHeight[is.na(tmpHeight)] <- 1 # Make sure we not end up with NA.
		tmpYmax <- max(tmpHeight)
    if(tmpYmax==0){tmpYmax <- 1} # Make Y max at least 1.
		myYval <- c(myYval, 
				rep(tmpYmax, length(unique(data$Marker[data$Dye==ci]))))
	}

	# Debug info.
	if(debugInfo){
	  print("data:")
	  print(data)
	  print(str(data))
	  print("myDye:")
	  print(myDye)
	  print("myXmin:")
	  print(myXmin)
	  print("myXmax:")
	  print(myXmax)
	  print("myYval:")
	  print(myYval)
	  print("myText:")
	  print(myText)
	  print("markerSpace:")
	  print(markerSpace)
	  flush.console()
	} else {}  
  
	# Expand the plot (by increasing the Y values) to make room for marker ranges.
	myYval <- as.numeric(myYval) * markerSpace

	# Create annotation data frame for loci.
	markerRanges <- data.frame(Dye=factor(myDye),	# Facet.
				Color=myDye,			# Dye.
				Xmin=myXmin,			# Marker lower range.	
				Xmax=myXmax,			# Marker upper range.
				Size=(myXmin+myXmax)/2,		# Midpoint of marker range.
				Height=myYval,			# Maximum peak height per channel + margin.
				Text=myText)			# Marker names.

	# Debug info.
	if(debugInfo){
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
	myPlot <- myPlot + geom_rect(aes_string(xmin="Xmin", xmax="Xmax",
          ymin="Height", ymax="Height"),
					alpha = .2, data=markerRanges, fill="blue", color="red")

	# Add marker names.
	myPlot <- myPlot + geom_text(aes_string(label="Text", y="Height"), 
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
						data=alleleInfo, size=2, vjust=0)

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
	if(debugInfo){
		print("EXIT: generateEPG")
		flush.console()
	} else {}

	return(myPlot)

}
################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 04: Roxygenized.
# 03: Added if-case to return correct data type. stringsAsFactors=FALSE
# 02: New parameter 'colName'
# 01: First version.

#' @title List to data frame
#'
#' @description
#' \code{listToDataframe} convert list to data frame.
#'
#' @details
#' Convert DNA profile information in list format to data frame format.
#' Returns the data binded to the respective marker/loci name.
#' 
#' @param data a list with alleles.
#' @param kit string or integer indicating the amplification kit used.
#' @param colName string specifying desired column name of the output data frame.
#' 
#' @return data.frame with corresponding alleles.
#' 



listToDataframe <- function(data, kit, colName=NA){

	# Create a data frame for the result.
	newData <- data.frame(NA, NA)

	# Set names.
	names(newData) <- c(colName, "Marker")

	# Remove all NAs
	newData <- newData[-1,]

	# Get locus names.
	markers <- getKit(kit)$locus

	# Check if matching data.
	if(length(data)==length(markers)){

		# Loop over all data in list.
		for(mi in seq(along=data)){

			# Unlist and convert to character.
			if(is.character(unlist(data))){
				cData <- as.character(unlist(data[mi]))
			} else if(is.numeric(unlist(data))){
				cData <- as.numeric(unlist(data[mi]))
			} else {
				cData <- unlist(data[mi])
			}

			# Repeat loci for each data.
			cMarker <- rep(markers[mi], length(cData))

			# Create a data frame.
			tmpdf <- data.frame(cData , Marker=cMarker, stringsAsFactors = FALSE)

			# Add column names.
			names(tmpdf) <- c(colName, "Marker")

			# Add result to new data frame.	
			newData <- rbind(newData, tmpdf)

		}

	} else {

		warning("length of list must be equal to number of markers in kit")
	}

	return(newData)

}
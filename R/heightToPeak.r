################################################################################
# TODO LIST
# TODO: Handle factors...

################################################################################
# CHANGE LOG
# 06: Some extra error handling. Handles negative samples when keepNA=FALSE.
# 05: Parameter 'keepNA'
# 04: Parameter debugInfo added.
# 03: Roxygenized.
# 02: Warning for no 'Size' or 'Height' column.
# 01: First version

#' @title Height to peak.
#'
#' @description
#' \code{heightToPeak} 'converts' a peak into a plottable polygon.
#'
#' @details
#' 'Converts' a single height and size value to a plottable 0-height-0 triangle/peak value.
#' Makes 3 data points from each peak size for plotting a polygon representing a peak.
#' Factors in other columns might get converted to factor level.
#' 
#' @param data data frame containing at least columns 'Height' and 'Size'.
#' @param width numeric specifying the width of the peak in bp.
#' @param keepNA logical, TRUE keep empty markers.
#' 
#' @return data.frame with new values.
#' 
#' @keywords internal

heightToPeak <- function(data, width=1, keepNA=TRUE, debugInfo=FALSE){

  # Debug info.
  if(debugInfo){
    print("ENTER: heightToPeak")
    print("data:")
    print(data)
    print(str(data))
    flush.console()
  } else {}

  # CHECK ARGUMENTS -----------------------------------------------------------
  
  if(!is.data.frame(data)){
    stop("'data' must be a data frame.",
         call. = TRUE)
  }
  
  if(!any(grepl("Height",names(data)))){
		stop("Data frame 'data' must contain a column 'Height'.",
			call. = TRUE)
	}

	if(!any(grepl("Size",names(data)))){
		stop("Data frame 'data' must contain a column 'Size'.",
			call. = TRUE)
	}

  if(!is.numeric(width)){
    stop("Peak width 'width' must be a numeric value.",
         call. = TRUE)
  }
  
  if(!is.logical(keepNA)){
    stop("'keepNA' must be a logical value.",
         call. = TRUE)
  }

  # FUNCTION ------------------------------------------------------------------
  
  if(!keepNA){
    # Remove all rows with no height.
    data <- data[!is.na(data$Height),]
  } else if (keepNA){
    # Replace all NAs with 0s.
    data$Height[is.na(data$Height)] <- 0
  }

	# Create an empty data frame 3 times the length of 'data'.
	newData <- data.frame(matrix(NA,nrow(data)*3,ncol(data)))

	# Add column names.
	names(newData) <- names(data)

	# Initiate row counter.
	r <- 1

  # Do not enter loop if no rows.
  if(nrow(data) > 0){
    
  	# Loop through the data frame.
   	for(i in 1:nrow(data)){
  
    		# Copy data and change value of x1 and y1.
    		newData[r,] <- data[i,]
    		newData$Size[r] <- newData$Size[r] - width/2
    		newData$Height[r] <- 0
    		r <- r + 1
    
    		# Copy data for x2 and y2.
    		newData[r,] <- data[i,]
    		r <- r + 1
    
    		# Copy data and change value of x3 and y3.
    		newData[r,] <- data[i,]
    		newData$Size[r] <- newData$Size[r] + width/2
    		newData$Height[r] <- 0
    		r <- r + 1
        
  	}
  }

  # Debug info.
  if(debugInfo){
    print("EXIT: heightToPeak")
    print("newData:")
    print(newData)
    print(str(newData))
    flush.console()
  } else {}
  

	return(newData)

}

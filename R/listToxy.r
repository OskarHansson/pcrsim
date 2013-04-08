################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 02: Roxygenized and changed name from list2xy to listToxy
# 01: First version.

#' @title List to xy values
#'
#' @description
#' \code{listToxy} combines matching x values to a list of y values.
#'
#' @details
#' Combines a list of Y values with a vector of matching X values into a 
#' data frame with x and y values suitable for plotting.
#' 
#' @param ylist list of y values.
#' @param xval list of matching x values.
#' @param elements logical, TRUE length equals number of elements in each list
#' FALSE, length equals number of sublists.
#' 
#' @return data.frame with columns 'X' and 'Y'.
#' 


listToxy <- function(ylist, xval, elements=TRUE){

	tmp <- NULL

	# Check that all vectors in list have the same length.
	if(elements){
		repX <- unlist(unique(lapply(ylist,FUN=length)))
	} else {
		repX <- length(ylist)
	}

	# Unlist Y value.
	ylist <- unlist(ylist)

	# Create X value vector.
	if(length(repX) > 1){
		# Different number of points at each X value.
		for(i in seq(along=repX)){
			tmp<-c(tmp, rep(xval[i], repX[i]))
		}
		xval <- tmp
	} else {
		# Same number of points at each X value.
		xval <- lapply(xval,FUN=rep, times=repX)
		xval <- unlist(xval)
	}

	# Create result data frame.
	res <- data.frame("Y"=ylist,"X"=xval)

	return(res)

}

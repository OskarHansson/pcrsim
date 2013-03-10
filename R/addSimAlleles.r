################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 02: Roxygenized.
# 01: First version

#' @title Add simulated alleles
#'
#' @description
#' \code{addSimAlleles} Adds the peak height from the two simulated alleles
#' from simPCR and returns a list of the total peak heights for the simulated
#' marker.
#'
#' @details
#' 'data' contains short tandem repeat (STR) marker/locus names in the required
#'   column. Based on the provided kit name other information is attached to
#'   'data' in new columns.
#'   
#' @param data List returned from \code{simPCR}.
#' 
#' @return list the totel peak height for each marker.
#' 
#' @keywords internal
#' @export 
#' @examples
#' # Create test data.
#' x <- list(c(250, 260), c(123,109))
#' addSimAlleles(x)

addSimAlleles <- function(data){
# Adds the peak height of the two simulated alleles together.

	totalPh <- list()
	for(i in seq(along=data)){

		totalPh[[i]] <- data[[i]][[1]] + data[[i]][[2]]
	}

	return(totalPh)

}

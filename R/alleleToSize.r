################################################################################
# TODO LIST
# TODO: ...

################################################################################
# CHANGE LOG
# 04: Added column 'Size' before loop.
# 03: Roxygenized.
# 02: Added if condition for 'slim' and option 'debugInfo'.
# 01: First version

#' @title Allele to size
#'
#' @description
#' \code{alleleToSize}  Converts allele names to fragment length in base pairs.
#'
#' @details
#' Estimates the fragment size in base pair based on allele names
#' (i.e. number of repeats).
#' Handles 'X' and 'Y' by replacing them with '1' and '2'.
#' Handles microvariant notation (e.g. '9.3').
#' 
#' @param data data frame with at least 'Marker' and 'Allele' columns.
#' @param kit String or number indicating the amplification kit used.
#' @param debugInfo logical for printing debug information.
#' 
#' @return data.frame with an additional column 'Size'.
#' 
#' @keywords internal
#' 
#' @seealso getKit


alleleToSize <- function(data, kit, debugInfo=FALSE){
  
  require(strvalidator)
  
  # Debug info.
  if(debugInfo){
    print("ENTER: alleleToSize")
    flush.console()
  } else {}

  if(length(grep("Allele", names(data))) == 0){
    stop("'data' must contain at least one column 'Allele*'",
         call. = TRUE)
  }

  if(length(grep("Marker", names(data))) == 0){
    stop("'data' must contain a column 'Marker'",
         call. = TRUE)
  }

  if(length(grep("Allele", names(data))) > 1){
    
    if(length(grep("Sample.Name", names(data))) == 0){
      stop("'data' in 'wide' format must contain a column 'Sample.Name'",
           call. = TRUE)
    }
    
  }
  
  # Get kit.
  kitInfo <- getKit(kit)
  marker <- unique(kitInfo$locus)
  offset <- kitInfo$offset
  repeatUnit <- kitInfo$repeatUnit
  
  # Debug info.
  if(debugInfo){
    print("kit:")
    print(kit)
    print("marker:")
    print(marker)
    print("data:")
    print(data)
    flush.console()
  } else {}
  
  # Flatten data?
  if(length(grep("Allele", names(data))) > 1){
    
    # Slim data.
    data <- slim(data=data, fix=c("Sample.Name","Marker"), stack=c("Allele"))
    
    warning(paste("Converting 'wide' to 'long' format.",
                  "Columnes other than 'Sample.Name', 'Marker', and 'Allele*'",
                  "is discarded!"), call. = FALSE)
    
    # Debug info.
    if(debugInfo){
      print("Data 'slimmed':")
      print(data)
      flush.console()
    } else {}
  }

  # Add a 'Size' column.
  data$Size <- NA
  
  # Loop over all markers.
  for (m in seq(along=marker)){
    
    # Get alleles for current marker.
    allele <- unique(as.character(data$Allele[data$Marker == marker[m]]))
    
    # Remove NAs.
    allele <- allele[!is.na(allele)]
    
    # Copy to temporary working variable.
    alleleTmp <- allele
    
    # Loop over all alleles.
    for(a in seq(along=allele)){
      
      # Make selection.
      sel <- data$Marker == marker[m] & data$Allele == as.character(allele[a])
      
      # Check presence of X/Y.
      indexOfXY <- grep("[X,x,Y,y]", allele)
      if (length(indexOfXY) > 0) {
        alleleTmp <- toupper(allele)
        # Use 1 and 2 for X and Y.
        alleleTmp <- sub("X", 1, alleleTmp)
        alleleTmp <- sub("Y", 2, alleleTmp)
      }
      
      # Convert to numeric.
      alleleTmp <- as.numeric(alleleTmp)
      
      # Calculate estimated size.
      data$Size[sel] <- offset[m] + 
        floor(alleleTmp[a]) * repeatUnit[m] + 
        (alleleTmp[a] %% 1) * 10
      
    }
  }
  
  # Debug info.
  if(debugInfo){
    print("Result:")
    print(data)
    print("EXIT: alleleToSize")
    flush.console()
  } else {}

  return (data)
  
}

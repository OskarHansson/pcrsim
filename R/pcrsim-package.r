###############################################################################
#' Forensic DNA process simulator.
#'
#' PCRsim is a package for simulating the forensic DNA process. \code{pcrsim}
#' opens up a graphical user interface which allow the user to enter parameters
#' required for the simulation. Once calibrated the program can be used to:
#' reduce the laboratory work needed for validation of new STR kits,
#' help develop methods for interpretation of DNA evidence, etc.
#' This is a first version which is still experimental/under development.   
#' 
#' \tabular{ll}{
#' Package: \tab pcrsim\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-03-09\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#' 
#' @title Simulation of the forensic DNA process
#' @docType package
#' @name pcrsim-package
#' @author Oskar Hansson \email{oskar.hansson@@fhi.no}
#' @section Warning: This package is experimental and has not been thoroughly validated.
#' @import  ggplot2 data.table gWidgets RGtk2 strvalidator
#' @keywords package
#' @references  Gill, Peter, James Curran, and Keith Elliot.
#' \\u0022 A Graphical Simulation Model of the Entire DNA Process Associated with
#'  the Analysis of Short Tandem Repeat Loci\\u0022
#'  Nucleic Acids Research 33, no. 2 (2005): 632-643. doi:10.1093/nar/gki205.
#'   
NULL

#' ESX17 Positive Control Profile
#' 
#' A dataset in 'GeneMaper' format containing the DNA profile of
#' the ESX17 positive control sample with homozygotes as one entry.
#' 
#' @docType data
#' @keywords datasets
#' @name ref1
#' @usage data(ref1)
#' @format A data frame with 17 rows and 4 variables
NULL

#' ESX17 Positive Control Profile
#' 
#' A dataset in 'GeneMaper' format containing the DNA profile of
#' the ESX17 positive control sample with homozygotes as two entries.
#' 
#' @docType data
#' @keywords datasets
#' @name ref11
#' @usage data(ref11)
#' @format A data frame with 17 rows and 4 variables
NULL


#' Typing data in 'GeneMapper' format
#' 
#' A dataset containing ESX17 genotyping result for 8 replicates
#' of the positive control sample, a negative control and ladder.
#' 
#' @docType data
#' @keywords datasets
#' @name set1
#' @usage data(set1)
#' @format A data frame with 170 rows and 13 variables
NULL

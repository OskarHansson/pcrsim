% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcrsim.r
\name{pcrsim}
\alias{pcrsim}
\title{GUI for PCRsim}
\usage{
pcrsim(debug = FALSE)
}
\arguments{
\item{debug}{logical, indicating if debug information should be printed.}
}
\description{
A graphical user interface for simulation of the entire DNA process.
}
\details{
This graphical user interface give access to parameters and functions
for simulation of the forensic DNA process.
Detailes are entered for each sub process organised into tabs.
Simulation is performed and the result can be viewed within the GUI,
plotted as an electropherogram (EPG), or saved to a text file.
The EPG can be saved as an image.
}
\examples{
\dontrun{
# Open the graphical user interface.
pcrsim()
}

}

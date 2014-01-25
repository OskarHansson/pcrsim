\name{generateEPG}
\alias{generateEPG}
\title{Generate EPG}
\usage{
generateEPG(data, kit, plotTitle = NULL, peaks = TRUE, debug = FALSE)
}
\arguments{
  \item{data}{data frame containing columns 'Allele',
  'Marker' and 'Height'.}

  \item{kit}{string or integer representing the STR typing
  kit.}

  \item{plotTitle}{string providing the title for the EPG.}

  \item{peaks}{logical, TRUE plot peaks using mean peak
  height for distributions.}

  \item{debug}{logical for printing debug information to
  the console.}
}
\value{
NULL.
}
\description{
\code{generateEPG} visualises an EPG from DNA profiling
data.
}
\details{
Generates a electropherogram like plot from 'data' and
'kit'. Homozygotes should be entered as a single entry.
}
\examples{
# Load package.
library(pcrsim)
library(strvalidator)
# Load genotyping data.
data("set1")
# Extract a sample.
mySample <- set1[set1$Sample.Name=="PC1",]
# Generate an electropherogram like plot.
generateEPG(data=mySample, kit="ESX17")
}
\keyword{internal}


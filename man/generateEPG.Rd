\name{generateEPG}
\alias{generateEPG}
\title{Generate EPG}
\usage{
  generateEPG(data, kit, plotTitle = NULL, peaks = TRUE,
    debugInfo = FALSE)
}
\arguments{
  \item{data}{data frame containing columns 'Allele',
  'Marker' and 'Height'.}

  \item{kit}{string or integer representing the STR typing
  kit.}

  \item{plotTitle}{string providing the title for the EPG.}

  \item{peaks}{logical, TRUE plot peaks using mean peak
  height for distributions.}

  \item{debugInfo}{logical for printing debug information
  to the console.}
}
\value{
  NULL.
}
\description{
  \code{generateEPG} visualises an EPG from DNA profiling
  data.
}
\details{
  ggplot2 version of plotEPG. Require packages 'ggplot2'
  and 'data.table'. Generates a electropherogram like plot
  from 'data' and 'kit'.
}
\examples{
data("set1")
mySample <- set1[set1$Sample.Name=="PC1",]
generateEPG(data=mySample, kit="ESX17")
}
\keyword{internal}


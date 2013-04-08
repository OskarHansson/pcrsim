\name{addSimAlleles}
\alias{addSimAlleles}
\title{Add simulated alleles}
\usage{
  addSimAlleles(data)
}
\arguments{
  \item{data}{List returned from \code{simPCR}.}
}
\value{
  list the totel peak height for each marker.
}
\description{
  \code{addSimAlleles} Adds the peak height from the two
  simulated alleles from simPCR and returns a list of the
  total peak heights for the simulated marker.
}
\details{
  'data' contains short tandem repeat (STR) marker/locus
  names in the required column. Based on the provided kit
  name other information is attached to 'data' in new
  columns.
}
\examples{
# Create test data.
x <- list(c(250, 260), c(123,109))
# Add simulated heterozygote peak heights to a total peak height for each locus.
addSimAlleles(x)
}
\keyword{internal}


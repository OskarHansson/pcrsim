\name{alleleToSize}
\alias{alleleToSize}
\title{Allele to size}
\usage{
alleleToSize(data, kit, debugInfo = FALSE)
}
\arguments{
  \item{data}{data frame with at least 'Marker' and
  'Allele' columns.}

  \item{kit}{String or number indicating the amplification
  kit used.}

  \item{debugInfo}{logical for printing debug information.}
}
\value{
data.frame with an additional column 'Size'.
}
\description{
\code{alleleToSize} Converts allele names to fragment
length in base pairs.
}
\details{
Estimates the fragment size in base pair based on allele
names (i.e. number of repeats). Handles 'X' and 'Y' by
replacing them with '1' and '2'. Handles microvariant
notation (e.g. '9.3').
}
\seealso{
getParameter
}
\keyword{internal}


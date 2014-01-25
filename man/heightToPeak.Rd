\name{heightToPeak}
\alias{heightToPeak}
\title{Height to peak.}
\usage{
heightToPeak(data, width = 1, keepNA = TRUE, debugInfo = FALSE)
}
\arguments{
  \item{data}{data frame containing at least columns
  'Height' and 'Size'.}

  \item{width}{numeric specifying the width of the peak in
  bp.}

  \item{keepNA}{logical, TRUE keep empty markers.}
}
\value{
data.frame with new values.
}
\description{
\code{heightToPeak} 'converts' a peak into a plottable
polygon.
}
\details{
'Converts' a single height and size value to a plottable
0-height-0 triangle/peak value. Makes 3 data points from
each peak size for plotting a polygon representing a peak.
Factors in other columns might get converted to factor
level.
}
\keyword{internal}


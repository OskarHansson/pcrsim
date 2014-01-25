\name{getParameter}
\alias{getParameter}
\title{Get kit parameters}
\usage{
getParameter(kitNameOrIndex = NULL, showMessages = FALSE, .kitInfo = NULL,
  debug = FALSE)
}
\arguments{
  \item{kitNameOrIndex}{string or integer specifying the
  kit.}

  \item{showMessages}{logical, default TRUE for printing
  messages to the R promt.}
}
\value{
list with kit information.
}
\description{
\code{getParameter} provides information about STR kits.
}
\details{
The function returns the following information for a kit
specified in kits.txt: Short kit name, Full kit name,
Marker/locus names, Dye for each marker/locus, Start offset
(in base pairs) for each marker, Size of repeating unit (in
base pairs) for each marker.  If no matching kit or kit
index is found NA is returned.  If NULL a vector of
available kits is printed and NA returned.
}
\examples{
getParameter("ESX17")
}
\keyword{internal}


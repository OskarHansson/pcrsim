% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simProfile.r
\name{simProfile}
\alias{simProfile}
\title{DNA Profile Simulator}
\usage{
simProfile(
  data = NULL,
  kit = NULL,
  sim = 1,
  name = NULL,
  db = NULL,
  debug = FALSE
)
}
\arguments{
\item{data}{data.frame with columns 'Marker' and 'Allele' (creates fixed profiles).}

\item{kit}{character string to specify the typing kit (if kit=NULL all markers in db will be used).}

\item{sim}{integer to specify the number of replicates or random profiles.}

\item{name}{character string giving the sample base name ('Sim' is appended).}

\item{db}{data.frame with the allele frequency database (used if data=NULL to create random profiles).}

\item{debug}{logical TRUE to indicate debug mode.}
}
\value{
data.frame with columns 'Sample.Name', 'Marker', 'Allele', 'Sim'.
}
\description{
Simulates DNA profiles.
}
\details{
There are three ways to create DNA profiles:
1) Simulate DNA profiles of the selected kit using allele frequencies from
the provided db.
2) Simulate DNA profiles using all available markers and allele frequencies
from the provided db.
3) Provide a data.frame with a fixed DNA profile.
\code{sim} random profiles or \code{sim} replicates are created.
The resulting data.frame can be used as input to \code{\link{simSample}}.
NB! Homozygous alleles must be specified two times e.g. 16, 16.
}
\examples{
# Create a data frame with a DNA profile.
markers = rep(c("D3S1358","TH01","FGA"), each=2)
alleles = c(15,18,6,10,25,25)
df <- data.frame(Marker=markers, Allele=alleles)

# Simulate sample
res <- simProfile(data=df, sim=10, name="Test")
print(res)
}
\seealso{
\code{\link{simSample}}
}

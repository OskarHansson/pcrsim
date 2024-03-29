% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simSample.r
\name{simSample}
\alias{simSample}
\title{Forensic Sample Simulator}
\usage{
simSample(
  data,
  cells = NULL,
  sd.cells = 0,
  conc = NULL,
  sd.conc = 0,
  vol = NULL,
  sd.vol = 0,
  cell.dna = 0.006,
  haploid = FALSE,
  kit = NULL,
  slope = NULL,
  intercept = NULL,
  debug = FALSE
)
}
\arguments{
\item{data}{data.frame with columns 'Marker', 'Allele', and 'Sim' defining
the DNA profiles and simulation id (counter).
Preferably output from \code{\link{simProfile}}.}

\item{cells}{integer for the estimated number of cells.}

\item{sd.cells}{numeric for the standard deviation of \code{cells}.}

\item{conc}{numeric for the estimated DNA concentration.}

\item{sd.conc}{numeric for the standard deviation of \code{conc}.}

\item{vol}{numeric for the estimated sample volume.}

\item{sd.vol}{numeric for the standard deviation of \code{vol}.}

\item{cell.dna}{numeric to indicate the DNA content of a diploid cell in
nanograms (ng).}

\item{haploid}{logical TRUE to indicate haploid cells.}

\item{kit}{character string defining the DNA typing kit used to calculate
allele size (used to calculate allele sizes needed for the regression option
 i.e. \code{slope} and \code{intercept}).}

\item{slope}{numeric from regression of log concentration by fragment
size (bp).}

\item{intercept}{numeric from regression of log concentration by fragment
size (bp).}

\item{debug}{logical TRUE to indicate debug mode.}
}
\value{
data.frame with simulated result in columns 'Cells'.
}
\description{
Simulates the DNA content in forensic stain.
}
\details{
Simulates the number of DNA molecules in a forensic stain either
from:
1) An estimate of the number of cells in the stain.
2) The DNA concentration.
3) The slope and intercept values as obtained from log-linear regression
of DNA concentration by size in basepair.
The regression emulates degradation and should not be used together with
simulation of degradation using \code{\link{simDegradation}}.
Some parameters accept vectors so that simulated samples can have different
number of cells and be a mixture of haploid and diploid samples (see examples).
Note 1: Number of cells can be decimal values since it is an estimate.
Note 2: Number of cells will always be integer if haploid=TRUE because
binomial selection require integer values.
Note 3: To get the same total amount of DNA in samples with diploid and haploid cells.
the parameter for haploid cells must be: cells = 2 * number_of_diploid_cells
NB! Important that each marker has two rows (i.e. homozygotes is e.g. 16, 16).
}
\examples{
# Create a data frame with a DNA profile.
markers = rep(c("D3S1358","TH01","FGA"), each=2)
alleles = c(15,18,6,10,25,25)
df <- data.frame(Marker=markers, Allele=alleles)
# Simulate profile.
prof <- simProfile(data=df, sim=3, name="Test")

# Simulate diploid sample.
res <- simSample(data=prof, cells=100, sd.cells=20)
print(res)

# Simulate haploid sample.
res <- simSample(data=prof, cells=100, sd.cells=20, haploid=TRUE)
print(res)

# Simulate haploid sample from concentration.
res <- simSample(data=prof, conc=0.02, sd.conc=0.001, vol=100, haploid=TRUE)
print(res)

# Simulate sample from slope and intercept.
res <- simSample(data=prof, vol=100, slope=-0.01, intercept=0.20, kit="SGMPlus")
print(res)

# Simulate mixture of diploid and haploid sample types of two concentrations.
res <- simSample(data=prof, cells=c(1000,1000,250), haploid=c(FALSE,TRUE,FALSE))
print(res)
}
\seealso{
\code{\link{simProfile}}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simPCR.r
\name{simPCR}
\alias{simPCR}
\title{PCR Simulator}
\usage{
simPCR(
  data,
  kit = NULL,
  method = "DEFAULT",
  pcr.cyc = 30,
  pcr.prob = 0.8,
  sd.pcr.prob = 0,
  stutter = TRUE,
  stutter.prob = 0.002,
  sd.stutter.prob = 0,
  stutter.reg = FALSE,
  vol.aliq = 10,
  sd.vol.aliq = 0,
  vol.pcr = 25,
  sd.vol.pcr = 0,
  debug = FALSE
)
}
\arguments{
\item{data}{data.frame with simulated data. Preferably output from \code{\link{simExtraction}}.
Required columns are 'Marker', 'Allele', 'Sim', Volume', and 'DNA'.}

\item{kit}{string representing the typing kit used, or data.frame with kit characteristics. 
Provides locus specific PCR efficiency and stutter probabilities.
If NULL \code{pcr.prob} and \code{stutter.prob} will be used for all loci.}

\item{method}{string representing the method of the specified kit.}

\item{pcr.cyc}{numeric for the number of PCR cycles.}

\item{pcr.prob}{numeric for the PCR efficiency (probability amplifying a DNA molecule).
Only used if \code{kit} is NULL.}

\item{sd.pcr.prob}{numeric for the standard deviation of \code{pcr.prob}.}

\item{stutter}{logical to simulate stutters.}

\item{stutter.prob}{numeric for the probability generating a stutter.
Only used if \code{kit} is NULL.}

\item{sd.stutter.prob}{numeric for the standard deviation of \code{stutter.prob}.}

\item{stutter.reg}{logical to use regression for stutter probability.}

\item{vol.aliq}{numeric for the aliquot extract forwarded to PCR.}

\item{sd.vol.aliq}{numeric for the standard deviation of \code{vol.aliq}.}

\item{vol.pcr}{numeric for the total PCR reaction volume.}

\item{sd.vol.pcr}{numeric for the standard deviation of \code{vol.pcr}.}

\item{debug}{logical to print debug information.}
}
\value{
data.frame with simulation results in columns 'PCR.Pip.Vol', 'PCR.Aliq.Prob',
'PCR.DNA', 'PCR.Vol', 'PCR.Prob', 'PCR.Prob.Stutter', 'PCR.Amplicon', 'PCR.Stutter.1',
'PCR.Stutter.2', and updated 'DNA' column (added if needed).
}
\description{
Simulates the Polymerase Chain Reaction (PCR) process.
}
\details{
Simulates the PCR process by a series of binomial distributions,
or multinomial distributions if \code{stutter=TRUE}. The PCR probability/
efficiency can be specified globally or per locus. Probability of stutter
formation can be specified globally, per locus, or per locus and allele size.
}
\examples{
# Create a data frame with a DNA profile.
markers = rep(c("D3S1358","TH01","FGA"), each=2)
alleles = c(15,18,6,10,25,25)
df <- data.frame(Marker=markers, Allele=alleles)

# Simulate profile.
smpl <- simProfile(data=df, sim=10)

# Simulate sample.
smpl <- simSample(data=smpl, cells=1000, sd.cells=200)

# Simulate extraction.
smpl <- simExtraction(data=smpl, vol.ex=200, sd.vol=10, prob.ex=0.6, sd.prob=0.1)

# Simulate PCR with 95\% PCR efficiency and 0.2\% stutter probability for all loci.
res <- simPCR(data=smpl, pcr.prob=0.95, pcr.cyc=30, vol.aliq=10,
 sd.vol.aliq=0.1, vol.pcr=25, sd.vol.pcr=1)

# Simulate PCR with locus specific PCR efficiency and stutter probability.
res <- simPCR(data=smpl, kit="ESX17", pcr.cyc=30, vol.aliq=10,
 sd.vol.aliq=0.1, vol.pcr=25, sd.vol.pcr=1)

# Simulate PCR with locus specific PCR efficiency and stutter probability.
res <- simPCR(data=smpl, kit="ESX17", pcr.cyc=30, vol.aliq=10,
 sd.vol.aliq=0.1, vol.pcr=25, sd.vol.pcr=1, stutter.reg=TRUE)
}
\seealso{
\code{\link{simExtraction}}
}

\name{findPCRprob}
\alias{findPCRprob}
\title{Find PCR probabilities}
\usage{
  findPCRprob(sim, targetLb, acceptedDev = 0.01,
    stepSize = 0.001, seed = 0.75, maxPCRprob = 0.95,
    progress = FALSE, ...)
}
\arguments{
  \item{sim}{integer for number of simulations.}

  \item{targetLb}{numeric vector with target interlocus
  balances.}

  \item{acceptedDev}{numeric, accepted deviation from
  target.}

  \item{stepSize}{numeric, probPCR is changed by this
  value.}

  \item{seed}{numeric, start value for optimisation of
  probPCR.}

  \item{maxPCRprob}{numeric, maximal value for optimised
  PCR efficiency.}

  \item{progress}{logical, print progress to console.}

  \item{...}{arguments to be passed to
  \code{\link{simPCR}}}
}
\value{
  data.frame with additional columns 'N' and 'Mean'.
}
\description{
  \code{findPCRprob} Find values for the PCR efficiency
  parameter for simPCR that satisfy the target locus
  balances.
}
\details{
  The interlocus balance for a kit should be characterised
  during the internal validation of the kit. This function
  search for PCR efficiency parameters that upon PCR
  simulation result in similar locus balances.
}
\examples{
\dontrun{
# Experimental inter locus balances for the STR kit one wants to simulate.
target <- c(0.05,0.05,0.06,0.04,0.04,
             0.04,0.06,0.05,0.05,
             0.04,0.04,0.05,0.05,
             0.15,0.07,0.09,0.07)

# Find PCR efficiency values that upon simulation
# satisfy the experimental data for 0.5 ng of input DNA.
set.seed(10) # For reproducibility.
findPCRprob(sim=40, targetLb=target, ncells=83, progress=FALSE)

# The PCR efficency parameters can now be plugged into the PCR simulation model.
}
}


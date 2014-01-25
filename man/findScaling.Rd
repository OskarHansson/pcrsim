\name{findScaling}
\alias{findScaling}
\title{Find scaling factor}
\usage{
findScaling(sim, targetH, acceptedDev = 100, stepSize = 1, seed = 0,
  maxRounds = 10000, progress = FALSE, tDetect = 1.25 * 10^6, cyc = 30,
  ncells = 83, debug = FALSE)
}
\arguments{
  \item{sim}{integer for number of simulations.}

  \item{targetH}{numeric vector with target average peak
  height.}

  \item{acceptedDev}{numeric, accepted deviation from
  target.}

  \item{stepSize}{numeric, scaling is changed by this
  value.}

  \item{seed}{numeric, start value for optimisation of
  scaling.}

  \item{maxRounds}{numeric, maximal number of iterations.}

  \item{progress}{logical, print progress to console.}

  \item{tDetect}{numeric, detection threshold.}

  \item{cyc}{integer, number of PCR cycles.}

  \item{ncells}{integer, number of cells.}

  \item{debug}{logical, print debug information to
  console.}
}
\value{
numeric vector with scaling factors.
}
\description{
\code{findScaling} Find a peak height scaling value for
simPCR that satisfy the target average peak height.
}
\details{
Experimental function.
}


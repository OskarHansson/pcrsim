% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrateScaling.r
\name{calibrateScaling}
\alias{calibrateScaling}
\title{Calibrate Peak Height Scaling}
\usage{
calibrateScaling(
  data,
  ref = NULL,
  target,
  c.min = 0,
  c.max = 10000,
  min.step.size = 1e-04,
  exact.matching = FALSE,
  progress = FALSE,
  .i = 1
)
}
\arguments{
\item{data}{data.frame simulated data.}

\item{ref}{data.frame with reference profiles for the simulated data.
If NULL the best guess will be used (see \code{\link{guessProfile}}).}

\item{target}{numeric target average peak height 'H'.}

\item{c.min}{numeric the smallest correction factor in the current range.}

\item{c.max}{numeric the largest correction factor in the current range.}

\item{min.step.size}{numeric threshold. Exit function when step size is this small.}

\item{exact.matching}{logical to indicate exact reference sample to dataset sample name matching.}

\item{progress}{logical flag to show progress messages.}

\item{.i}{integer internal counter for number of iterations.}
}
\value{
numeric corrected scaling intercept.
}
\description{
Corrects the peak height scaling intercept.
}
\details{
Finds the scaling factor correction that gives the best fit between the
simulated peak heights and the target average peak height.
This is the final step in calibrating pcrsim for a specific kit and method.
}

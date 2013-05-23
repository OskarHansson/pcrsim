\name{simPCR}
\alias{simPCR}
\title{PCR simulator}
\usage{
  simPCR(ncells, probEx = 1, probAlq = 1, probPCR = 1,
    cyc = 28, tDetect = 5 * 10^7, dip = TRUE, KH = 55,
    sim = 1)
}
\arguments{
  \item{ncells}{integer for number of cells/dna molecules
  for the current allele. Can be vector of integers giving
  the number of cells in each simulation.}

  \item{probEx}{numeric for probability that an allele
  survives the extraction (extraction efficiency). Can be
  vector of numeric giving the probability in each
  simulation.}

  \item{probAlq}{numeric, probability that an allele is
  aliquoted into the PCR reaction. Can be vector of numeric
  giving the probability in each simulation.}

  \item{probPCR}{numeric, probability that an allele is
  amplified during a PCR cycle (PCR efficiency). Can be
  vector of numeric giving the probability in each
  simulation.}

  \item{cyc}{integer, number of PCR cycles.}

  \item{tDetect}{integer, detection threshold. Number of
  molecules needed to trigger a signal.}

  \item{KH}{integer, correlation factor for number of
  molecules to peak height.}

  \item{sim}{integer, number of simulations.}

  \item{dip}{logical flagging for diploid cells (haploid
  cells are currently not implemented)}
}
\value{
  list with simulation results.
}
\description{
  \code{simPCR} simulates the PCR process.
}
\details{
  Based on \code{simPCR2} from the \code{forensim} package
  by Hinda Haned. Simulates PCR for a single allele by a
  series of binomial distributions.
}
\examples{
simPCR(ncells=100, probEx=0.7, probAlq=0.1, probPCR=0.85, sim=100)
}


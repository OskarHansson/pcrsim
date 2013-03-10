\name{simPCRexplorer}
\alias{simPCRexplorer}
\title{Explore PCR simulator}
\usage{
  simPCRexplorer(ncells = 100, probEx = 1, probAlq = 1,
    probPCR = 1, cyc = 28, tDetect = 2 * 10^7, dip = TRUE,
    KH = 55, simulations = 1000)
}
\arguments{
  \item{ncells}{number of cells.}

  \item{probEx}{probability of extraction.}

  \item{probAlq}{probability of aliquote.}

  \item{probPCR}{probability of PCR (PCR efficiency).}

  \item{cyc}{number of PCR cycles.}

  \item{tDetect}{detection threshold.}

  \item{KH}{scaling factor for number of molecules to peak
  height.}

  \item{simulations}{number of simulations.}

  \item{dip}{logical flagging for diploid cells (haploid
  cells are currently not implemented)}
}
\value{
  list with simulation results.
}
\description{
  \code{simPCRexplorer} explore \code{simPCR} by varying a
  parameter.
}
\details{
  Makes it easy to explore the effect of one parameter on
  the PCR simulation.
}
\examples{
# Simulating 0-1000 cells 1000 times each and plotting means.
sim <- simPCRexplorer(ncells=seq(0,1001, by=10), probEx=0.7, probAlq=0.1, probPCR=0.85)
plot(x=seq(0,1001, by=10), y=lapply(sim, mean), xlab=c("# cells"), ylab=c("Peak height (rfu)"))
}


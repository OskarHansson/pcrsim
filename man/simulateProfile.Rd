\name{simulateProfile}
\alias{simulateProfile}
\title{Simulate DNA profiles}
\usage{
  simulateProfile(alleles, ncells = NA, ncells.sd = 0,
    conc = NA, conc.sd = 0, intercept = NA,
    intercept.sd = 0, slope = NA, slope.sd = 0,
    exprob = NA, exprob.sd = 0, volume = NA, volume.sd = 0,
    aliq = NA, aliq.sd = 0, simulations = 1, kit = NA,
    celldna = 0.006, ...)
}
\arguments{
  \item{alleles}{list of alleles per marker.}

  \item{ncells}{numeric, number of cells ( = number of DNA
  molecules of each allele).}

  \item{ncells.sd}{numeric, standard deviation.}

  \item{conc}{numeric, human DNA concentration in DNA
  extract.}

  \item{conc.sd}{numeric, standard deviation.}

  \item{intercept}{numeric, degradation intercept.}

  \item{intercept.sd}{numeric, standard deviation for
  degradation intercept.}

  \item{slope}{numeric, degradation slope.}

  \item{slope.sd}{numeric, standard deviation for
  degradation slope.}

  \item{exprob}{numeric [0-1], probability that an allele
  survivives the extraction (extraction efficiency).}

  \item{exprob.sd}{numeric, standard deviation for the
  extraction efficiency.}

  \item{volume,}{numeric, final extraction volume.}

  \item{volume.sd,}{numeric, standard deviation for the
  final extraction volume.}

  \item{aliq}{numeric, volume of DNA extract taken for PCR
  amplification.}

  \item{aliq.sd}{numeric, standard deviation for volume of
  DNA extract.}

  \item{kit}{string or integer specifying an STR typing
  kit.}

  \item{simulations}{integer, number of simulations.}

  \item{celldna,}{numeric giving the DNA content of a
  cell.}

  \item{...}{arguments to be passed to
  \code{\link{simPCR}}}
}
\value{
  list with simulation results.
}
\description{
  \code{simulateProfile} simulates entire DNA profiles.
}
\details{
  Simulates the peak heights for a single person profile.
  Integration of degradation and interlocus balance. Handle
  missing alleles. Only single person profiles,
  unique(alleles) will be applied.
}
\examples{
# Define a DNA profile.
dnaProfile <- list(c("X","Y"),c(17,18),c(6,9.3),c(29,31.2),c(16,18),
c(13,15),c(12,13),c(22,25),c(9,13),
c(16,16),c(16,19),c(14,15),c(20,23),
c(10,14),c(18,23),c(13,14),c(15,16))

# Simulate.
simulateProfile(alleles=dnaProfile, kit="ESX17", simulations=5,
 ncells=200, ncells.sd=20,
 exprob=0.7, exprob.sd=0.1,
 volume=200, volume.sd=20,
 aliq=17.5, aliq.sd=1)
}


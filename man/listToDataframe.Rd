\name{listToDataframe}
\alias{listToDataframe}
\title{List to data frame}
\usage{
  listToDataframe(data, kit, colName = NA)
}
\arguments{
  \item{data}{a list with alleles.}

  \item{kit}{string or integer indicating the amplification
  kit used.}

  \item{colName}{string specifying desired column name of
  the output data frame.}
}
\value{
  data.frame with corresponding alleles.
}
\description{
  \code{listToDataframe} convert list to data frame.
}
\details{
  Convert DNA profile information in list format to data
  frame format. Returns the data binded to the respective
  marker/loci name.
}


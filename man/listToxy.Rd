\name{listToxy}
\alias{listToxy}
\title{List to xy values}
\usage{
listToxy(ylist, xval, elements = TRUE)
}
\arguments{
  \item{ylist}{list of y values.}

  \item{xval}{list of matching x values.}

  \item{elements}{logical, TRUE length equals number of
  elements in each list FALSE, length equals number of
  sublists.}
}
\value{
data.frame with columns 'X' and 'Y'.
}
\description{
\code{listToxy} combines matching x values to a list of y
values.
}
\details{
Combines a list of Y values with a vector of matching X
values into a data frame with x and y values suitable for
plotting.
}


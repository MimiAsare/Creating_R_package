% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{rando}
\alias{rando}
\title{Sample from Vector or Dataframe}
\usage{
rando(x, n = 1, replace = T)
}
\arguments{
\item{x}{an atomic vector or a dataframe}

\item{n}{number of samples or rows to be returned (default is 1)}

\item{replace}{logical, indicating whether or not sampling should be done with replacement (default is TRUE)}
}
\value{
a vector or a dataframe
}
\description{
This function is a wrapper around \code{\link{sample}} that tests whether the input
is an atomic vector or a dataframe-like object and returns either n samples or n rows
as appropriate.
}
\examples{
rando(1:10, 3)
rando(mtcars, 5)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{classes}
\alias{classes}
\title{Returns a character vector containing the classes of each variable in a tibble x.}
\usage{
classes(x)
}
\arguments{
\item{x}{A tibble}
}
\value{
A character vector containing the classes of each variable in \code{x}
}
\description{
Returns a character vector containing the classes of each variable in a tibble x.
}
\examples{
my_data <- tibble::tribble(
   ~name, ~age, ~height_cm,
   "Alice", 25, 165,
   "Bob", 30, 180,
   "Charlie", 35, 175
)
classes(my_data)
# Output: [1] "character" "numeric"   "numeric"
}

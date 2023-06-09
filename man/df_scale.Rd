% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{df_scale}
\alias{df_scale}
\title{Scale numeric variables in a tibble}
\usage{
df_scale(x, center = T, scale = T)
}
\arguments{
\item{x}{A tibble or data frame}

\item{center}{Logical. Should the variables be centered (subtract the mean)?}

\item{scale}{Logical. Should the variables be scaled (divide by the standard deviation)?}
}
\value{
A tibble with scaled numeric variables
}
\description{
Scale numeric variables in a tibble
}
\examples{
my_data <- tibble::tribble(
   ~name, ~age, ~height_cm,
   "Alice", 25, 165,
   "Bob", 30, 180,
   "Charlie", 35, 175
)
df_scale(my_data)
}

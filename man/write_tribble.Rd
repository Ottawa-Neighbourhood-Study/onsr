% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{write_tribble}
\alias{write_tribble}
\title{Write data to an .R file as a tribble.}
\usage{
write_tribble(mydata, destfile = NA)
}
\arguments{
\item{mydata}{Input data in a tibble or dataframe.}

\item{destfile}{File to write. If it does not have a ".R" extension, one will be added.}
}
\value{
Returns the original data invisibly to enable piping.
}
\description{
Write data to an .R file as a tribble.
}

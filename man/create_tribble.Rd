% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_tribble.R
\name{create_tribble}
\alias{create_tribble}
\title{creates a boilerplate tribble based on provided names}
\usage{
create_tribble(names, nrows = 4)
}
\arguments{
\item{names}{column names}

\item{nrows}{fake data number of rows}
}
\description{
creates a boilerplate tribble based on provided names
}
\examples{
create_tribble(names(mtcars), nrows = 4)
}

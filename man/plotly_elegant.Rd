% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_elegant.R
\name{plotly_elegant}
\alias{plotly_elegant}
\title{plotly dark theme}
\usage{
plotly_elegant(
  plot,
  ...,
  margin = 3,
  background = "#1f1f1e",
  color = "#ffffff"
)
}
\arguments{
\item{plot}{plotly plot}

\item{margin}{plot margin}

\item{background}{plot background}

\item{color}{font color}
}
\description{
plotly dark theme
}
\examples{
mtcars \%>\%
  plot_ly(x = ~cyl, y = ~mpg) \%>\%
  plotly_elegant(
    xaxis = list(title = "hey"),
    yaxis = list(side = "right")
  )
}

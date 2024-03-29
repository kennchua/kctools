% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kc_tab.R
\name{kc_tab}
\alias{kc_tab}
\title{Tabulate frequency and relative frequency}
\usage{
kc_tab(data, tabvar, dropna = FALSE)
}
\arguments{
\item{data}{The data frame to tabulate}

\item{tabvar}{The variables to tabulate}

\item{dropna}{If TRUE then drops NA values; if FALSE then includes NA values}
}
\value{
A data frame with tabulation of counts and proportions.
}
\description{
One-way and two-way tabulations of provided variables
}
\examples{
\dontrun{
kc_tab(mtcars, c(vs)) # one-way tabulate
kc_tab(mtcars, c(vs, am)) # two-way tabulate
}
}

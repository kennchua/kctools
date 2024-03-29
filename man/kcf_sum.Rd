% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kcf_sum.R
\name{kcf_sum}
\alias{kcf_sum}
\title{Descriptive statistics}
\usage{
kcf_sum(data, sumvar, byvar = NULL)
}
\arguments{
\item{data}{The data frame to summarize}

\item{sumvar}{The variables to summarize; accommodates tidyselect helpers}

\item{byvar}{The grouping variable for grouped summaries}
}
\value{
A data frame with descriptive stats: each var is a row; respective statistics are in column.
}
\description{
Compute pre-selected set of descriptive statistics for provided vars, optionally by group. Speedboost via tidytable.
}
\examples{
\dontrun{
kcf_sum(mtcars, c(mpg, disp)) # summary stats for mpg and disp
kcf_sum(mtcars, c(mpg, disp), c(am)) # summary stats for mpg and disp by am
kcf_sum(mtcars, where(is.numeric)) # summary stats for all numeric vars
}
}

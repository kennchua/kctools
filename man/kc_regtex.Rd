% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kc_regtex.R
\name{kc_regtex}
\alias{kc_regtex}
\title{Export regression table to LaTeX file}
\usage{
kc_regtex(
  reglist,
  sep = FALSE,
  coef_map = NULL,
  gof_map = NULL,
  add_rows = NULL,
  mc_cores = 1,
  num_fmt = "\%.3f",
  fpath = NULL,
  fname = "~/Desktop/kc_mod_all.tex",
  fname_est = "~/Desktop/kc_mod_est.tex",
  fname_det = "~/Desktop/kc_mod_det.tex"
)
}
\arguments{
\item{reglist}{A list of regression output using base::lm() or fixest::feols()}

\item{sep}{Whether to output coefficient estimates separately from model diagnostics}

\item{coef_map}{Model estimates to report; using modelsummary's coef_map argument}

\item{gof_map}{Model diagnostics to report; using modelsummary's gof_map argument}

\item{add_rows}{Additional rows to be included; using modelsummary's add_rows argument}

\item{mc_cores}{Parallel computation of model diagnostics; using modelsummary's mc.cores argument}

\item{num_fmt}{Format of numeric values; using modelsummary's fmt argument}

\item{fpath}{File path for output; deprecated - use fname* exclusively}

\item{fname}{File name for output (both coefficients and diagnostics)}

\item{fname_est}{File name for output (coefficient estimates only)}

\item{fname_det}{File name for output (model diagnostics only)}
}
\description{
Export a list of regression output in tex format with options to separate model estimates from diagnostics; depends on modelsummary, stringr, and purrr.
}

#' Custom LaTeX output for regression results using modelsummary package.
#' @param reglist A list of regression output using base::lm() or fixest::feols()
#' @param sep Whether to output coefficients estimates separately from model diagnostics
#' @param outest Whether to output coefficients estimates; default is true
#' @param outdet Whether to output model diagnostics; default is true
#' @param xvar_raw Vector of raw variables names to keep
#' @param xvar_lbl Vector of labels for for raw variables to be kept
#' @param gof_stat Vector of diagnostics to be included
#' @param path File path for output
#' @param fname_est File name for output (coefficient estimates only)
#' @param fname_det File name for output (model diagnostics only)
#' @param fname File name for output (both coefficients and diagnostics)
#' @import modelsummary
#' @import stringr


kc_regtex <- function(reglist,
                      sep = FALSE, outest = TRUE, outdet = TRUE, xvar_raw = NULL, xvar_lbl = NULL, gof_stat = NULL,
                      path = "~/Desktop/", fname_est = "mod_est.tex", fname_det = "mod_det.tex", fname = "mod_all.tex") {

  # Numeric returned as plain rather than enclosed in \num{}
  options(modelsummary_format_numeric_latex = "plain")

  # Set which gof statistics appear
  mod_gof <-
    tibble::tribble(
      ~raw,               ~clean,                        ~fmt,
      "nobs",             "Observations",                  0,
      "nclusters",        "Number of clusters",            0,
      "r.squared",        "\\textit{R}-squared",           2,
      "adj.r.squared",    "Adjusted \\textit{R}-squared",  2,
      "pseudo.r.squared", "Pseudo \\textit{R}-squared",    2,
      "rmse",             "RMSE",                          3,
      "statistic.Weak.instrument", "IV F-stat",            1,
      "p.value.Weak.instrument", "Weak IV p-value",        3,
      "p.value.Wu.Hausman", "Wu-Hausman p-value",          3,
      "p.value.Sargan",   "Sargan p-value",                3) |>
    {\(df) if (!is.null(gof_stat)) filter(df, raw %in% gof_stat) else df}()


  # Variables to include and their labels
  if (is.null(xvar_raw) == TRUE ) {
    mod_xvar = NULL
  } else if (is.null(xvar_raw) == FALSE & is.null(xvar_lbl) == TRUE) {
    mod_xvar = xvar_raw
    #print(mod_xvar)
  } else if (is.null(xvar_raw) == FALSE & is.null(xvar_lbl) == FALSE) {
    names(xvar_lbl) <- xvar_raw
    mod_xvar = xvar_lbl
    #print(mod_xvar)
  }

  # Generate modelsummary output (if variables are supplied)
  mod_msummary = msummary(reglist,
                          stars = c("*" = 0.1, "**" = .05, "***" = 0.01),
                          coef_map = mod_xvar,
                          fmt = '%.3f',
                          gof_map = mod_gof,
                          output = "latex_tabular")

  # Function to extract body of latex code
  extract_tbl_body = function(x) {
    start <- stringr::str_locate(x, "\\\\midrule")[1]
    end <- stringr::str_locate(x, "\\\\bottomrule")[2]
    stringr::str_sub(x, start, end)
  }

  # Extract rows of estimates
  mod_est = mod_msummary |>
    extract_tbl_body() |>
    stringr::str_match("(?s)\\\\midrule(.*?)\\\\midrule") |>
    {\(est) est[[2]]}()
  # est[[2]] extracts second element of str_match; previously .[[2]] when using %>%
  # I use str_match() to get the string sandwiched bet. "\midrule"
  # (?s) is added to match strings that span across line breaks

  # Extract rows of table details
  mod_det = mod_msummary |>
    extract_tbl_body() |>
    stringr::str_match("(?s)(?:.*\\\\midrule){2}(.*?)\\\\bottomrule") |> # second midrule occurrence
    {\(det) det[[2]]}() # first element of str_match
  # det[[2]] extracts second element of str_match; previously .[[2]] when using %>%

  # Add rows for IV F-stat if desired; add flexibility for lists with non-2sls output
  if ("ivwald" %in% gof_stat) {
    wald_vec <- vector(mode = "character", length = length(reglist))

    for (v in seq_along(reglist)) {
      if (class(reglist[[v]]) != "fixest") {
        wald_vec[[v]] = ""
      } else if (class(reglist[[v]]) == "fixest") {
        if (is.na(fitstat(reglist[[v]], ~ ivwald1))) {
          wald_vec[[v]] = ""
        } else {
          fitstat(reglist[[v]], ~ ivwald1)$ivwald1$stat |> round(digits = 2)
        }
      }
    }

    mod_iv =  paste(paste(c("IV F-stat", wald_vec),
                          collapse = " & "),
                    "\\\\\n", sep = "")

    mod_det = paste(mod_det, mod_iv, sep = "")
  }


  # Output latex code for body of table
  # If sep == FALSE (default), estimates and details are saved in the same latex code
  # If sep == TRUE, estimates and details are saved in sepa

  if (sep == FALSE & outest == TRUE & outdet == TRUE) {
    cat(c(mod_est,
          "\\\\",
          mod_det), sep = "\n",
        file = paste0(path, fname))

  } else if ((sep == FALSE | sep == TRUE) & outest == TRUE & outdet == FALSE) {
    cat(c(mod_est), sep = "\n",
        file = paste0(path, fname_est))
  } else if ((sep == FALSE | sep == TRUE) & outest == FALSE & outdet == TRUE) {
    cat(c(mod_det), sep = "\n",
        file = paste0(path, fname_det))
  } else if (sep == TRUE & outest == TRUE & outdet == TRUE) {
    cat(c(mod_est), sep = "\n",
        file = paste0(path, fname_est))
    cat(c(mod_det), sep = "\n",
        file = paste0(path, fname_det))
  }
}
#' Export regression table to LaTeX file
#'
#' Output regression table in tex format with options to separate model estimates from diagnostics; depends on modelsummary, stringr, and purrr.
#' @param reglist A list of regression output using base::lm() or fixest::feols()
#' @param sep Whether to output coefficient estimates separately from model diagnostics
#' @param coef_lbl Model estimates to report; using modelsummary's coef_map argument
#' @param gof_lbl Model diagnostics to report; using modelsummary's gof_map argument
#' @param rows_lbl Additional rows to be included; using modelsummary's add_rows argument
#' @param mc_cores Parallel computation of model diagnostics; using modelsummary's mc.cores argument
#' @param fpath File path for output
#' @param fname File name for output (both coefficients and diagnostics)
#' @param fname_est File name for output (coefficient estimates only)
#' @param fname_det File name for output (model diagnostics only)
#' @import modelsummary
#' @import stringr
#' @import purrr

kc_regtex <- function(reglist,
                      sep = FALSE, # output estimates separately from diagnotics
                      coef_lbl = NULL,
                      gof_lbl = NULL,
                      rows_lbl = NULL,
                      mc_cores = NULL,
                      fpath = "~/Desktop/",
                      fname = "kc_mod_all.tex",
                      fname_est = "kc_mod_est.tex",
                      fname_det = "kc_mod_det.tex") {

  # Numeric returned as plain rather than enclosed in \num{}
  options(modelsummary_format_numeric_latex = "plain")


  # Add rows for IV F-stat if desired
  # if (ivwald == TRUE) {
  #   wald_vec <- vector(mode = "character", length = length(reglist))
  #
  #   for (v in seq_along(reglist)) {
  #     if (class(reglist[[v]]) != "fixest") {
  #       wald_vec[[v]] = ""
  #     } else if (class(reglist[[v]]) == "fixest") {
  #       if (is.na(fixest::fitstat(reglist[[v]], ~ ivwald1))) {
  #         wald_vec[[v]] = ""
  #       } else {
  #         wald_vec[[v]] =
  #           fixest::fitstat(reglist[[v]], ~ ivwald1)$ivwald1$stat |> round(digits = 2)
  #       }
  #     }
  #   }
  #
  #   if (is.null(rows_lbl)) {
  #     rows_lbl <- dplyr::as_tibble(t(c("IV F-statistic", wald_vec)))
  #     names(rows_lbl) <- c("term", paste("Model", 1:length(reglist)))
  #
  #   } else if (!is.null(rows_lbl)) {
  #     rows_iv <- dplyr::as_tibble(t(c("IV F-statistic", wald_vec)))
  #     names(rows_iv) <- c("term", paste("Model", 1:length(reglist)))
  #
  #     rows_lbl <- dplyr::bind_rows(rows_iv, rows_lbl)
  #   }
  # }


  # Generate modelsummary output (if variables are supplied)
  reg_tex = msummary(reglist,
                     stars = c("*" = 0.1, "**" = .05, "***" = 0.01),
                     coef_map = coef_lbl, #mod_xvar,
                     fmt = '%.3f',
                     gof_map = gof_lbl, #reg_gof,
                     add_rows = rows_lbl,
                     mc.cores = mc_cores,
                     output = "latex_tabular")

  # Function to extract body of latex code
  extract_tbl_body = function(x) {
    start <- stringr::str_locate(x, "\\\\midrule")[1]
    end <- stringr::str_locate(x, "\\\\bottomrule")[2]
    stringr::str_sub(x, start, end)
  }

  # Extract rows of estimates
  mod_est = reg_tex |>
    extract_tbl_body() |>
    stringr::str_match("(?s)\\\\midrule(.*?)\\\\midrule") |>
    {\(est) est[[2]]}()
  # est[[2]] extracts second element of str_match; previously .[[2]] when using %>%
  # I use str_match() to get the string sandwiched bet. "\midrule"
  # (?s) is added to match strings that span across line breaks

  # Extract rows of model diagnostics
  mod_det = reg_tex |>
    extract_tbl_body() |>
    stringr::str_match("(?s)(?:.*\\\\midrule){2}(.*?)\\\\bottomrule") |> # second midrule occurrence
    {\(det) det[[2]]}() # first element of str_match
  # det[[2]] extracts second element of str_match; previously .[[2]] when using %>%


  # Output latex code for body of table
  # If sep == FALSE (default), estimates and diagnostics are saved in the same latex code
  # If sep == TRUE, estimates and diagnostics are saved in separate files

  if (sep == TRUE) {
    cat(c(mod_est), sep = "\n",
        file = paste0(fpath, fname_est))
    cat(c(mod_det), sep = "\n",
        file = paste0(fpath, fname_det))

  } else if (sep == FALSE) {
    cat(c(mod_est,
          "\\\\",
          mod_det), sep = "\n",
        file = paste0(fpath, fname))
  }

}

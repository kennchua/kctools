#' Export regression output to Excel file
#'
#' Export a list of plots to individual sheets within an Excel file; depends on modelsummary, openxlsx, dplyr, and purrr.
#' @param reglist A list of regression output using base::lm() or fixest::feols()
#' @param sname A string for the sheet title
#' @param coef_lbl Model estimates to report; using modelsummary's coef_map argument
#' @param gof_lbl Model diagnostics to report; using modelsummary's gof_map argument
#' @param rows_lbl Additional rows to be included; using modelsummary's add_rows argument
#' @param ivwald Report Kleibergen-Paap F-stat; using fixest's ivwald statistic
#' @param fpath File path for output
#' @param fname File name for output
#' @param overwrite If TRUE then overwrites file; if FALSE then appends sheet on existing file
#' @import modelsummary
#' @import openxlsx
#' @import purrr
#' @import dplyr

kc_regxls <- function(reglist, sname = NULL,
                      coef_lbl = NULL, gof_lbl = NULL, rows_lbl = NULL, # gof_stat = NULL
                      note_lbl = NULL,
                      fpath = "~/Desktop/", fname = "kc_mod_all.xlsx",
                      ivwald = FALSE,
                      overwrite = TRUE) {

  # reg_gof <-
  #   list(
  #     nobs = list("raw" = "nobs", "clean" = "Observations", "fmt" = function(x) format(round(x, 3), big.mark=",")),
  #     nclusters = list("raw" = "nclusters", "clean" = "Number of clusters", "fmt" = function(x) format(round(x, 3), big.mark=",")),
  #     r.squared = list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 2),
  #     adj.r.squared = list("raw" = "adj.r.squared", "clean" = "Adjusted R-squared", "fmt" = 2),
  #     within.r.squared = list("raw" = "within.r.squared", "clean" = "Within R-squared", "fmt" = 2),
  #     rmse = list("raw" = "rmse", "clean" = "RMSE", "fmt" = function(x) format(round(x, 3), nsmall = 3, big.mark=",")),
  #     AIC = list("raw" = "AIC", "clean" = "AIC", "fmt" = function(x) format(round(x, 3), nsmall = 3, big.mark=",")),
  #     BIC = list("raw" = "BIC", "clean" = "BIC", "fmt" = function(x) format(round(x, 3), nsmall = 3, big.mark=",")),
  #     logLik = list("raw" = "logLik", "clean" = "LogL", "fmt" = function(x) format(round(x, 3), nsmall = 3, big.mark=",")),
  #     statistic.Weak.instrument = list("raw" = "statistic.Weak.instrument", "clean" = "IV F-stat", "fmt" = 3),
  #     p.value.Weak.instrument = list("raw" = "p.value.Weak.instrument", "clean" = "Weak IV p-value", "fmt" = 3),
  #     p.value.Wu.Hausman = list("raw" = "p.value.Wu.Hausman", "clean" = "Wu-Hausman p-value", "fmt" = 3),
  #     p.value.Sargan = list("raw" = "p.value.Sargan", "clean" = "Sargan p-value", "fmt" = 3)
  #   ) |>
  #   {\(df) if (!is.null(gof_stat)) purrr::keep(df, names(df) %in% gof_stat) else df}()


  # Add rows for IV F-stat if desired; add flexibility for lists with non-2sls output
  if (ivwald == TRUE) {
    wald_vec <- vector(mode = "character", length = length(reglist))

    for (v in seq_along(reglist)) {
      if (class(reglist[[v]]) != "fixest") {
        wald_vec[[v]] = ""
      } else if (class(reglist[[v]]) == "fixest") {
        if (is.na(fixest::fitstat(reglist[[v]], ~ ivwald1))) {
          wald_vec[[v]] = ""
        } else {
          wald_vec[[v]] =
            fixest::fitstat(reglist[[v]], ~ ivwald1)$ivwald1$stat |> round(digits = 2)
        }
      }
    }
    if (is.null(rows_lbl)) {
      rows_lbl <- dplyr::as_tibble(t(c("IV F-statistic", wald_vec)))
      names(rows_lbl) <- c("term", paste("Model", 1:length(reglist)))

    } else if (!is.null(rows_lbl)) {
      rows_iv <- dplyr::as_tibble(t(c("IV F-statistic", wald_vec)))
      names(rows_iv) <- c("term", paste("Model", 1:length(reglist)))

      rows_lbl <- dplyr::bind_rows(rows_iv, rows_lbl)
    }
  }




  # Generate modelsummary output (if variables are supplied)
  reg_df = modelsummary::modelsummary(reglist,
                                      stars = c("*" = 0.1, "**" = .05, "***" = 0.01),
                                      coef_map = coef_lbl,
                                      fmt = '%.3f',
                                      gof_map = gof_lbl, # reg_gof
                                      add_rows = rows_lbl,
                                      output = "data.frame") |>
    dplyr::select(-part, -statistic) |>
    dplyr::mutate(across(term, ~replace(., duplicated(.), NA))) |> # replace duplicate with NA
    dplyr::add_row(term = note_lbl)

  # Change name of df columns
  colnames(reg_df) <- c(" ", paste0("(", 1:(ncol(reg_df)-1), ")"))

  # Export to Excel
  if (overwrite == TRUE | (overwrite == FALSE & !file.exists(paste0(fpath, fname)))) {
    wb_regs <- openxlsx::createWorkbook()

    if (is.null(sname)) {
      sheetname = paste("Results", length(names(wb_regs)) + 1)
    } else if (!is.null(sname)) {
      sheetname = sname
    }

    openxlsx::addWorksheet(wb_regs, sheetname)
    openxlsx::writeData(wb_regs, sheetname, reg_df)

    openxlsx::addStyle(wb_regs, sheetname,
                       cols = 2:ncol(reg_df), rows = 1:(nrow(reg_df)+1), gridExpand = TRUE,
                       style = openxlsx::createStyle(halign = 'center')) # center alignment of columns

    openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = "auto")

    openxlsx::saveWorkbook(wb_regs,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  } else if (overwrite == FALSE & file.exists(paste0(fpath, fname))) {
    wb_regs <- openxlsx::loadWorkbook(paste0(fpath, fname))

    if (is.null(sname)) {
      sheetname = paste("Results", length(names(wb_regs)) + 1)
    } else if (!is.null(sname)) {
      if (sname %in% names(wb_regs)) stop("Sheet name must be unique.")
      sheetname = sname
    }

    openxlsx::addWorksheet(wb_regs, sheetname)
    openxlsx::writeData(wb_regs, sheetname, reg_df)

    openxlsx::addStyle(wb_regs, sheetname,
                       cols = 2:ncol(reg_df), rows = 1:(nrow(reg_df)+1), gridExpand = TRUE,
                       style = openxlsx::createStyle(halign = 'center')) # center alignment of columns

    openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = "auto")

    openxlsx::saveWorkbook(wb_regs,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  }

}

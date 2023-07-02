#' Export regression output to Excel file
#'
#' Export a list of regression output to a table within an Excel sheet; depends on modelsummary, openxlsx, dplyr, and purrr.
#' @param reglist A list of regression output using base::lm() or fixest::feols()
#' @param sname A string for the sheet title
#' @param coef_map Model estimates to report; using modelsummary's coef_map argument
#' @param gof_map Model diagnostics to report; using modelsummary's gof_map argument
#' @param add_rows Data frame of rows to be appended; using modelsummary's add_rows argument
#' @param add_notes A vector of table notes to append at the bottom of the table
#' @param cols_lbl A vector of column titles to appear on the top row of the table
#' @param mc_cores Parallel computation of model diagnostics; using modelsummary's mc.cores argument
#' @param num_fmt Format of numeric values; using modelsummary's fmt argument
#' @param fpath File path for output; deprecated - use fname exclusively
#' @param fname File name for output
#' @param overwrite If TRUE then overwrites file; if FALSE then appends sheet on existing file
#' @import modelsummary
#' @import openxlsx
#' @import purrr
#' @import dplyr
#' @import tibble

kc_regxls <- function(reglist,
                      sname = NULL, # sheet name
                      coef_map = NULL, # coefficients to display; formely coef_lbl
                      gof_map = NULL,  # gof stats to display; formerly gof_lbl
                      add_rows = NULL, # rows for diagnostics; formerly rows_lbl
                      add_notes = NULL, # table notes; formerly note_lbl
                      cols_lbl = NULL, # column header; default is numbers
                      mc_cores = 1, # parallel computation
                      num_fmt = '%.3f', # format of numbers
                      fpath = NULL,
                      fname = "~/Desktop/kc_mod_all.xlsx",
                      overwrite = TRUE) {


  # Generate modelsummary output as data frame
  reg_df = modelsummary::msummary(reglist,
                                  stars = c("*" = 0.1, "**" = .05, "***" = 0.01),
                                  coef_map = coef_map,
                                  gof_map = gof_map,
                                  mc.cores = mc_cores,
                                  fmt = num_fmt,
                                  output = "data.frame") |>
    # Keep coefficient and standard error
    dplyr::select(-part, -statistic) |>
    # Replace duplicated terms with NA
    dplyr::mutate(across(term, ~replace(., duplicated(.), NA))) |>
    # Append rows for table notes and diagnostics (if provided)
    {\(df) if (is.null(add_rows)) df else df |>
        rename_with(cols = everything(), ~ names(add_rows)) |> # rename columns
        bind_rows(add_rows)}() |>
    # Append table notes to first column
    {\(df) dplyr::bind_rows(df, tibble::as_tibble_col(add_notes,
                                                      column_name = names(df)[[1]]))}() |>
    # Rename columns to either default or user-specified values
    {\(df) if (!is.null(cols_lbl)) purrr::set_names(df, cols_lbl)
      else purrr::set_names(df,
                            c(" ", paste0("(", 1:(ncol(df)-1), ")")))}()


  # Rename columns: default vs. user-provided
  # if(is.null(cols_lbl)) purrr::set_names(reg_df,
  #                                       c(" ", paste0("(", 1:(ncol(reg_df)-1), ")")))
  # else if(!is.null(cols_lbl)) purrr::set_names(reg_df,
  #                                              cols_lbl)


  # Export to Excel
  ### If overwriting or if file exists...
  if (overwrite == TRUE | (overwrite == FALSE & !file.exists(paste0(fpath, fname)))) {
    wb_regs <- openxlsx::createWorkbook()

    if (is.null(sname)) {
      sheetname = paste("Results", length(names(wb_regs)) + 1)
    } else if (!is.null(sname)) {
      sheetname = sname
    }

    openxlsx::addWorksheet(wb_regs, sheetname, zoom = 150)
    openxlsx::writeData(wb_regs, sheetname, reg_df, startRow = 1, startCol = 1)


    # Borders of Column Title, Table Body
    openxlsx::addStyle(wb_regs, sheetname, style = createStyle(border = "Top"), rows = 1, cols = 1:ncol(reg_df))
    openxlsx::addStyle(wb_regs, sheetname, style = createStyle(border = "Bottom"), rows = 1, cols = 1:ncol(reg_df), stack = TRUE)

    openxlsx::addStyle(wb_regs, sheetname, style = createStyle(border = "Bottom"),
                       rows = nrow(reg_df)-length(add_notes) + 1, cols = 1:ncol(reg_df))

    # Center Alignment of Column Content
    openxlsx::addStyle(wb_regs, sheetname,
                       cols = 2:ncol(reg_df), rows = 1:(nrow(reg_df)+1), gridExpand = TRUE,
                       style = openxlsx::createStyle(halign = 'center'), stack = TRUE)

    # Width of first column
    openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = 22)

    openxlsx::saveWorkbook(wb_regs,  file = paste0(fpath, fname),
                           overwrite = TRUE)


  ### If NOT overwriting or if file does not exist...
  } else if (overwrite == FALSE & file.exists(paste0(fpath, fname))) {
    wb_regs <- openxlsx::loadWorkbook(paste0(fpath, fname))

    if (is.null(sname)) {
      sheetname = paste("Results", length(names(wb_regs)) + 1)
    } else if (!is.null(sname)) {
      if (sname %in% names(wb_regs)) stop("Sheet name must be unique.")
      sheetname = sname
    }

    openxlsx::addWorksheet(wb_regs, sheetname, zoom = 150)
    openxlsx::writeData(wb_regs, sheetname, reg_df, startRow = 1, startCol = 1)

    # Borders of Column Title, Table Body
    openxlsx::addStyle(wb_regs, sheetname, style = openxlsx::createStyle(border = "Top"), rows = 1, cols = 1:ncol(reg_df))
    openxlsx::addStyle(wb_regs, sheetname, style = openxlsx::createStyle(border = "Bottom"), rows = 1, cols = 1:ncol(reg_df), stack = TRUE)

    openxlsx::addStyle(wb_regs, sheetname, style = openxlsx::createStyle(border = "Bottom"),
                       rows = nrow(reg_df)-length(add_notes) + 1, cols = 1:ncol(reg_df))

    # Center Alignment of Column Content
    openxlsx::addStyle(wb_regs, sheetname,
                       cols = 2:ncol(reg_df), rows = 1:(nrow(reg_df)+1), gridExpand = TRUE,
                       style = openxlsx::createStyle(halign = 'center'), stack = TRUE)

    # Width of first column
    openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = 22)
    # if (is.null(add_notes)) {
    #   openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = "auto")
    # } else if (!is.null(add_notes)) {
    #   openxlsx::setColWidths(wb_regs, sheetname, cols = 1, widths = 20)
    # }

    openxlsx::saveWorkbook(wb_regs,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  }

}

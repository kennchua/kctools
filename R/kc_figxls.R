#' Export figures to Excel file
#'
#' Export a list of plots to individual sheets within an Excel file; depends on openxlsx and purrr.
#' @param plotlist List of plots to export
#' @param sname Vector of names for sheet title
#' @param width Width (in inches)
#' @param height Height (in inches)
#' @param fpath File path for output
#' @param fname File name for output
#' @param overwrite If TRUE then overwrites file; if FALSE then appends sheets on existing file
#' @import openxlsx
#' @import purrr

kc_figxls <- function(plotlist, sname = NULL,
                      width = 7.45, height = 5.21,
                      fpath = "~/Desktop/", fname = "KC_Plots.xlsx",
                      overwrite = TRUE) {

  if (overwrite == TRUE | (overwrite == FALSE & !file.exists(paste0(fpath, fname)))) {
    wb_plot <- openxlsx::createWorkbook()

    # Construct sheet name
    if (is.null(sname)) {
      sheetname = paste("Plot", 1:length(plotlist))
    } else if (!is.null(sname)) {
      sheetname = sname
    }

    if (length(plotlist) != length(sheetname)) stop("Number of plots must be equal to number of sheet names provided.")

    # Insert plot to worksheet
    purrr::walk2(plotlist, sheetname,
                 function(x, y) {
                   openxlsx::addWorksheet(wb_plot, toString(y), zoom = 125, gridLines = FALSE)
                   print(x)
                   openxlsx::insertPlot(wb_plot, toString(y), width = width, height = height, fileType = "png", units = "in")
                 })

    openxlsx::saveWorkbook(wb_plot,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  } else if (overwrite == FALSE & file.exists(paste0(fpath, fname))) {
    wb_plot <- openxlsx::loadWorkbook(paste0(fpath, fname))

    # Construct sheet names
    if (is.null(sname)) {
      sheetname = paste("Plot", length(names(wb_plot))+1:length(plotlist))
    } else if (!is.null(sname)) {
      sheetname = sname
    }

    if (length(plotlist) != length(sheetname)) stop("Number of plots must be equal to number of sheet names provided.")

    # Insert plot to worksheet
    purrr::walk2(plotlist, sheetname,
                 function(x, y) {
                   openxlsx::addWorksheet(wb_plot, toString(y), zoom = 125, gridLines = FALSE)
                   print(x)
                   openxlsx::insertPlot(wb_plot, toString(y), width = width, height = height, fileType = "png", units = "in")
                 })

    openxlsx::saveWorkbook(wb_plot,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  }

}

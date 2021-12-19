#' Export figures to Excel file
#'
#' Export a list of plots to individual sheets within an Excel file; depends on openxlsx and purrr.
#' @param plotlist List of plots to export
#' @param sname Vector of names for sheet title
#' @param fpath File path for output
#' @param fname File name for output
#' @param overwrite If TRUE then overwrites file; if FALSE then appends sheets on existing file
#' @import openxlsx
#' @import purrr

kc_figxls <- function(plotlist, sname,
                      fpath = "~/Desktop/", fname = "Plots.xlsx",
                      overwrite = TRUE) {

  if (length(plotlist) != length(sname)) stop("Number of plots must be equal to number of sheet names provided.")

  if (overwrite == TRUE | (overwrite == FALSE & !file.exists(paste0(fpath, fname)))) {
    wb_plot <- openxlsx::createWorkbook()

    purrr::walk2(plotlist, sname,
                 function(x, y) {
                   openxlsx::addWorksheet(wb_plot, toString(y), gridLines = FALSE)
                   print(x)
                   openxlsx::insertPlot(wb_plot, toString(y), width = 7.45, height = 5.21, fileType = "png", units = "in")
                 })

    openxlsx::saveWorkbook(wb_plot,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  } else if (overwrite == FALSE & file.exists(paste0(fpath, fname))) {
    wb_plot <- openxlsx::loadWorkbook(paste0(fpath, fname))

    purrr::walk2(plotlist, sname,
                 function(x, y) {
                   openxlsx::addWorksheet(wb_plot, toString(y), gridLines = FALSE)
                   print(x)
                   openxlsx::insertPlot(wb_plot, toString(y), width = 7.45, height = 5.21, fileType = "png", units = "in")
                 })

    openxlsx::saveWorkbook(wb_plot,  file = paste0(fpath, fname),
                           overwrite = TRUE)

  }

}

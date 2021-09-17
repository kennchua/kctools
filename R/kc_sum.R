#' Compute descriptive statistics for provided vars, optionally by group.
#' @param data The data frame to summarize.
#' @param sumvar The variables to summarize.
#' @param byvar The variables to group by. (optional)
#' @param ... Optional. Columns in the data frame
#' @return A data frame with descriptive statistics: each var is a row; respective stats are in column.
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
#' @examples
#' \dontrun{
#' kc_sum(mtcars, c(mpg, disp))
#' kc_sum(mtcars, c(mpg, disp), c(vs, am))
#' }

kc_sum <- function(data, sumvar, byvar = NULL) {
  data |>
    dplyr::group_by(across({{ byvar }})) |>
    dplyr::select({{ sumvar }}) |>
    dplyr::select(where(is.numeric)) |>
    dplyr::summarize(across(everything(),
                            list(avg = ~ mean(., na.rm = TRUE),
                                 sdv = ~ sd(., na.rm = TRUE),
                                 nobs = ~ dplyr::n(),
                                 nna = ~ sum(is.na(.)),
                                 min = ~min(., na.rm = TRUE),
                                 max = ~max(., na.rm = TRUE),
                                 p05 = ~quantile(., 0.05, na.rm = TRUE),
                                 p25 = ~quantile(., 0.25, na.rm = TRUE),
                                 p50 = ~quantile(., 0.50, na.rm = TRUE),
                                 p75 = ~quantile(., 0.75, na.rm = TRUE),
                                 p95 = ~quantile(., 0.95, na.rm = TRUE)),
                            .names = "{fn}_{col}")) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = -{{ byvar }},
                        names_pattern = "(.*)_(.*)",
                        names_to = c("stat", "varname"),
                        values_to = c("value")) |>
    tidyr::pivot_wider(names_from = "stat", values_from = c("value"))
}

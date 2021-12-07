#' Descriptive statistics
#'
#' Compute pre-selected set of descriptive statistics for provided vars, optionally by group.
#' @param data The data frame to summarize.
#' @param sumvar The variables to summarize; accommodates tidyselect helpers.
#' @param byvar The variables to group by. (optional)
#' @return A data frame with descriptive stats: each var is a row; respective statistics are in column.
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @examples
#' \dontrun{
#' kc_sum(mtcars, c(mpg, disp)) # summary stats for vars mpg and disp
#' kc_sum(mtcars, c(mpg, disp), c(vs)) # summary stats for vars mpg and disp by vs
#' kc_sum(mtcars, where(is.numeric)) # summary stats for all numeric vars
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

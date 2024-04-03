#' Descriptive statistics
#'
#' Compute pre-selected set of descriptive statistics for provided vars.
#' @param data The data frame to summarize
#' @param sumvar The variables to summarize; accommodates tidyselect helpers
#' @return A data frame with descriptive stats: each var is a row; respective statistics are in column.
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @examples
#' \dontrun{
#' kc_sum(mtcars, c(mpg, disp)) # summary stats for vars mpg and disp
#' kc_sum(mtcars, where(is.numeric)) # summary stats for all numeric vars
#' }

kc_sum <- function(data, sumvar) {

  grpvar <- dplyr::group_vars(data)

  data |>
    dplyr::select({{ sumvar }}) |>
    dplyr::select(where(is.numeric)) |>
    dplyr::summarize(across(everything(),
                            list(avg = ~ mean(., na.rm = TRUE),
                                 sdv = ~ sd(., na.rm = TRUE),
                                 nobs = ~ dplyr::n(),
                                 nna = ~ sum(is.na(.)),
                                 min = ~min(., na.rm = TRUE),
                                 max = ~max(., na.rm = TRUE),
                                 p01 = ~quantile(., 0.01, na.rm = TRUE),
                                 p05 = ~quantile(., 0.05, na.rm = TRUE),
                                 p25 = ~quantile(., 0.25, na.rm = TRUE),
                                 p50 = ~quantile(., 0.50, na.rm = TRUE),
                                 p75 = ~quantile(., 0.75, na.rm = TRUE),
                                 p95 = ~quantile(., 0.95, na.rm = TRUE),
                                 p99 = ~quantile(., 0.99, na.rm = TRUE)),
                            .names = "{fn}_{col}")) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = -{{grpvar}},
                        names_pattern = "([^_]+)_(.*)",
                        names_to = c("kctools_stat", "kctools_varname"),
                        values_to = c("kctools_value")) |>
    tidyr::pivot_wider(names_from = "kctools_stat", values_from = c("kctools_value")) |>
    dplyr::relocate(avg:p99, .after = kctools_varname)
}

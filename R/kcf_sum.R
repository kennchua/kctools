#' Descriptive statistics
#'
#' Compute pre-selected set of descriptive statistics for provided vars, optionally by group. Speedboost via tidytable.
#' @param data The data frame to summarize
#' @param sumvar The variables to summarize; accommodates tidyselect helpers
#' @param byvar The grouping variable for grouped summaries
#' @return A data frame with descriptive stats: each var is a row; respective statistics are in column.
#' @import tidytable
#' @examples
#' \dontrun{
#' kcf_sum(mtcars, c(mpg, disp)) # summary stats for mpg and disp
#' kcf_sum(mtcars, c(mpg, disp), c(am)) # summary stats for mpg and disp by am
#' kcf_sum(mtcars, where(is.numeric)) # summary stats for all numeric vars
#' }

kcf_sum <- function(data, sumvar, byvar = NULL) {

  data |>
    tidytable::select({{ byvar }}, {{ sumvar }}) |>
    tidytable::select({{ byvar }}, where(is.numeric)) |>
    tidytable::summarize(tidytable::across(everything(),
                                           list(avg = ~ mean(., na.rm = TRUE),
                                                sdv = ~ sd(., na.rm = TRUE),
                                                nobs = ~ tidytable::n.(),
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
                                           .names = "{fn}_{col}"),
                         .by = {{ byvar }}) |>
    tidytable::pivot_longer(cols = -{{ byvar }},
                            names_pattern = "([^_]+)_(.*)",
                            names_to = c("stat", "varname"),
                            values_to = c("value")) |>
    tidytable::pivot_wider(names_from = "stat", values_from = c("value")) |>
    tidytable::relocate(avg, sdv, nobs,
                        nna, min, max, p01:p99, .after = varname)
}

.datatable.aware <- TRUE


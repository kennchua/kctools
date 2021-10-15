#' Tabulate provided vars to display count and proportion. Speedboost via tidytable.
#' @param data The data frame to tabulate.
#' @param tabvar The variables to tabulate.
#' @return A data frame with tabulation of counts and proportions.
#' @import tidytable
#' @examples
#' \dontrun{
#' kcf_tab(mtcars, c(vs)) # one-way tabulate
#' kcf_tab(mtcars, c(vs, am)) # two-way tabulate
#' }


kcf_tab <- function(data, tabvar, dropna = FALSE) {
  data |>
    {\(df) if (dropna == TRUE) tidytable::filter.(df, tidytable::across.(c({{ tabvar }}), ~ !is.na(.))) else df}() |>
    tidytable::count.({{ tabvar }}) |>
    tidytable::rename.(n = N) |>
    tidytable::mutate.(prop = n/sum(n))
}
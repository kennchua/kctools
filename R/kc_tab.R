#' Tabulate frequency and relative frequency
#'
#' One-way and two-way tabulations of provided variables
#' @param data The data frame to tabulate.
#' @param tabvar The variables to tabulate.
#' @return A data frame with tabulation of counts and proportions.
#' @import dplyr
#' @examples
#' \dontrun{
#' kc_tab(mtcars, c(vs)) # one-way tabulate
#' kc_tab(mtcars, c(vs, am)) # two-way tabulate
#' }


kc_tab <- function(data, tabvar, dropna = FALSE) {
  data |>
    {\(df) if (dropna == TRUE) dplyr::filter(df, dplyr::across(c({{ tabvar }}), ~ !is.na(.))) else df}() |>
    dplyr::group_by(across({{ tabvar }})) |> # when passing multiple group_by arguments...
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(prop = n/sum(n))
}

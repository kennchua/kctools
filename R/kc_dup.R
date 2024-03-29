#' Indicators for duplicated rows
#'
#' Generate columns with count of occurrences, tag for duplication, and running count of occurrences
#' @param data The data frame to examine for duplicates
#' @param dupvar The variables to check for duplicates
#' @return A data frame with counts per group, tag of whether duplicated, counter within group.
#' @import dplyr
#' @examples
#' \dontrun{
#' kc_dup(mtcars, c(vs))
#' kc_dup(mtcars, c(vs, am))
#' }


kc_dup <- function(data, dupvar) {
  data |>
    dplyr::group_by(across({{ dupvar }})) |>
    dplyr::add_count(name = "dup_times") |> # number of times group appears
    dplyr::mutate(dup_tag = dplyr::if_else(dup_times > 1, 1, 0)) |> # whether duplicated
    dplyr::mutate(dup_counter = dplyr::row_number()) |> # counter of the row number within group
    dplyr::ungroup() |>
    dplyr::select({{ dupvar }}, starts_with("dup_")) |>
    dplyr::arrange(dplyr::across({{ dupvar }}))
}

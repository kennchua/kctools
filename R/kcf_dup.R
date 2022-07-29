#' Indicators for duplicated rows
#'
#' Generate columns with count of occurrences, tag for duplication, and running count of occurrences. Speedboost via tidytable.
#' @param data The data frame to examine for duplicates
#' @param dupvar The variables to check for duplicates
#' @return A data frame with counts per group, tag of whether duplicated, counter within group.
#' @import tidytable
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' kcf_dup(mtcars, c(vs))
#' kcf_dup(mtcars, c(vs, am))
#' }


kcf_dup <- function(data, dupvar) {
  data |>
    tidytable::mutate.(dup_times = n.(), .by = {{ dupvar }}) |>
    tidytable::mutate.(dup_tag = ifelse(dup_times > 1, 1, 0)) |> # whether duplicated
    tidytable::mutate.(dup_counter = tidytable::cur_group_rows.(), .by = {{ dupvar }}) |> # counter of the row number within group
    tidytable::select.({{ dupvar }}, starts_with("dup_")) |>
    tidytable::arrange.(tidytable::across.({{ dupvar }}))
}

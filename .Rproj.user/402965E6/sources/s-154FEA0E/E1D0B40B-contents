#' Create data frame containing pairwise distances of points in sf objects x and y.
#' @param x an sf object
#' @param y an sf object
#' @param idx id variable for each row in x
#' @param idy id variable for each row in y
#' @return A data frame containing pairwise distances of each row in x and y
#' @import dplyr
#' @import sf
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_replace

st_pairwise_dist <- function(x, y, idx, idy) {
  if(!inherits(x, "sf") | !inherits(y, "sf")) {
    stop("x and y must be sf objects")
  }

  dist_tbl = sf::st_distance(x, y, by_element = FALSE) |>
    dplyr::as_tibble()

  dist_prws = dplyr::bind_cols(x |> sf::st_set_geometry(NULL) |> dplyr::select({{ idx }}),
                               dist_tbl) |>
    dplyr::rename_with(~ stringr::str_replace(., "V", "dist_"), starts_with("V")) |>
    tidyr::pivot_longer(cols = starts_with("dist_"),
                        names_pattern = "(.*)_(.*)",
                        names_to = c("lbl", "idvar"),
                        values_to = "distance") |>
    dplyr::left_join(y |> sf::st_set_geometry(NULL) |> select({{ idy }}) |>
                       dplyr::mutate(idvar = as.character(dplyr::row_number())),
                     by = "idvar") |>
    dplyr::select(-lbl, -idvar) |>
    dplyr::relocate(distance, .after = last_col())

  return(dist_prws)
}



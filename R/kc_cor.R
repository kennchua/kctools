#' Compute correlation matrix and display lower triangular and diagonal only.
#' @param data The data frame to use.
#' @param corvar The variables to correlate.
#' @return Either a data frame or matrix of correlations.
#' @import dplyr
#' @examples
#' \dontrun{
#' kc_cor(mtcars, c(mpg, disp, wt)) # returns df correlations of mpg, disp, and wt
#' kc_cor(mtcars, starts_with("c"), df = FALSE) # returns matrix of correlations of vars beginning in c
#' }


kc_cor <- function(data, corvar, df = TRUE) {
  cor_mat = data |>
    dplyr::select({{ corvar }}) |>
    cor(use = "complete", method = c("pearson"))

  cor_mat[upper.tri(cor_mat, diag = FALSE)] = NA

  cor_df = dplyr::as_tibble(cor_mat, rownames = "var")

  if (df == TRUE) {
    return(cor_df)
  } else if (df == FALSE) {
    return(cor_mat)
  }
}

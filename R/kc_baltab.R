#' Create balance table with group means and standard deviations or standard error; conduct t-test and F-test
#'
#' Compute group means, standard deviations, as well as standard errors and conducts difference-in-means test with respect to reference group
#' @param data The data frame to tabulate
#' @param balvar The variables to tabulate
#' @param grpvar If TRUE then drops NA values; if FALSE then includes NA values
#' @param refgrp Reference category for group comparisons
#' @param fevar Name of fixed effects or block variables to condition on
#' @param vcov Standard error type (e.g. "hc1", "iid", "hetero") or a name of a variable to cluster standard errors on
#' @param report_sd If TRUE then reports standard deviation of each variable; if FALSE then reports standard errors
#' @param report_ttest_pval If TRUE then reports p-values in difference in means test; if FALSE then reports mean difference
#' @return A data frame with tabulation of counts and proportions.
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import fixest
#' @import modelsummary




kc_baltab <- function(data, balvar, grpvar, refgrp,
                      fevar = NULL, vcov = "hc1",
                      report_sd = FALSE, report_ttest_pval = TRUE) {

  if (!is.data.frame(data))
    stop("Data must be a data frame.")

  if (!all(balvar %in% (names(data))))
    stop("Covariates must be in data.")

  if (!grpvar %in% (names(data)))
    stop("Group variable must be in data.")

  if (!refgrp %in% (data |> dplyr::pull({{grpvar}}) |> unique()))
    stop("Reference group must be in group variable.")

  # Note on FE:
  if (!is.null(fevar))
    if(!fevar %in% (names(data)))
      stop("Fixed effects must be in data.")

  # Note on SE:
  if (vcov %in% c("iid", "normal", "standard", "hetero", "white", "hc1")) {
    vcov_type = vcov
  } else if (! vcov %in% c("iid", "normal", "standard", "hetero", "white", "hc1") &
             vcov %in% names(data)) {
    vcov_type = vcov_cluster(vcov)
  } else if (! vcov %in% c("iid", "normal", "standard", "hetero", "white", "hc1") &
             ! vcov %in% names(data)) {
    stop("SE type must be a variable in data or a recognized type.")
  }

  # Drop NA group category
  main_data <- data |>
    tidyr::drop_na(grpvar)

  grpvar_fct <- main_data |>
    dplyr::pull(grpvar) |>
    unique() |>
    sort() |>
    factor()

  nonref <- setdiff(main_data |> dplyr::pull({{grpvar}}) |> unique(),
                    refgrp)

  # Setup
  gm <- list(
    list("raw" = "nobs", "clean" = "nobs", "fmt" = function(x) format(round(x, 3), big.mark=","))
  )


  # Compute mean / sd or mean / se
  if (report_sd == TRUE) {
    mean_sd <- main_data |>
      dplyr::group_by(dplyr::across({{grpvar}})) |>
      dplyr::summarize(dplyr::across(balvar,
                                     list(mean = \(x) mean(x, na.rm = TRUE),
                                          sdev = \(x) sd(x, na.rm = TRUE),
                                          nobs = \(x) sum(!is.na(x))))) |>
      tidyr::pivot_longer(cols = ends_with("mean") | ends_with("sdev") | ends_with("nobs"),
                          values_to = "vals",
                          names_to = "vars") |>
      dplyr::mutate(stat = stringr::str_sub(vars, -4, -1)) |>
      dplyr::mutate(vars = stringr::str_remove(vars, "_[^_]*$")) |> # everything after underscore
      tidyr::pivot_wider(names_from = grpvar, values_from = "vals") |>
      dplyr::mutate(vars_fct = factor(vars, levels = balvar)) |>
      dplyr::mutate(stat_fct = factor(stat, levels = c("mean", "sdev", "nobs"))) |>
      dplyr::arrange(vars_fct, stat_fct) |>
      dplyr::select(vars_fct, stat_fct, all_of(grpvar_fct)) |>
      dplyr::mutate(dplyr::across(all_of(grpvar_fct), ~ dplyr::case_when(stat_fct == "mean" ~ sprintf("%.3f", round(., digits = 3)),
                                                                         stat_fct == "sdev" ~ paste0("(", sprintf("%.3f", round(., digits = 3)), ")"),
                                                                         stat_fct == "nobs" ~ paste0("[",  format(as.integer(.), big.mark = ","), "]")))) |>
      dplyr::select(-stat_fct) |>
      # Replace duplicates with NA
      dplyr::mutate(vars_fct = replace(vars_fct, duplicated(vars_fct), NA))


    #return(mean_sd)
  } else if (report_sd == FALSE) {
    # Compute mean, SE, nobs
    mean_se <- main_data |>
      dplyr::group_by(dplyr::across({{grpvar}})) |>
      tidyr::nest() |>
      # Specify variables to get mean and SE
      dplyr::mutate(covar = purrr::map2(data, .data[[grpvar]],
                                 \(d, g)  purrr::map(balvar, \(y) y))) |>
      # Run regressions to get mean and SE
      dplyr::mutate(reg_mean_se =  purrr::map2(data, covar,
                                       \(d, y) fixest::feols(.[y] ~ 1,
                                                             fixef = NULL,
                                                             data = d,
                                                             vcov = vcov_type))) |>
      # Get data frame with mean, se, and nobs using modelsummary
      dplyr::mutate(mean_se_nobs = purrr::map(reg_mean_se,
                                       \(res) modelsummary::msummary(res,
                                                                     output = "data.frame",
                                                                     gof_map = gm))) |>
      # Rename objects
      dplyr::mutate(mean_se_nobs = purrr::map(mean_se_nobs,
                                       \(d) d |>
                                         dplyr::mutate(reg_stats = dplyr::case_when(part == "estimates" & statistic == "estimate" ~ "mean",
                                                                                    part == "estimates" & statistic == "std.error" ~ "serr",
                                                                                    part == "gof" & statistic == "" ~ "nobs")) |>
                                         dplyr::select(reg_stats, starts_with("lhs")))) |>
      # Keep only objects
      dplyr::select({{grpvar}}, mean_se_nobs) |>
      tidyr::unnest(c(mean_se_nobs)) |>
      # Pivot so that grpvar is in column header
      tidyr::pivot_wider(names_from = reg_stats, values_from = starts_with("lhs")) |>
      tidyr::pivot_longer(cols = -1) |> # all but the first column
      tidyr::pivot_wider(names_from = grpvar, values_from= "value") |>
      # Arrange Mutate num obs format
      tidyr::separate(name, into = c("vars", "stat"), sep="_(?=[^_]+$)") |>
      dplyr::mutate(vars = str_remove(vars, "lhs: ")) |>
      dplyr::mutate(vars_fct = factor(vars, levels = balvar)) |>
      dplyr::mutate(stat_fct = factor(stat, levels = c("mean", "serr", "nobs"))) |>
      dplyr::arrange(vars_fct, stat_fct) |>
      dplyr::select(vars_fct, stat_fct, all_of(grpvar_fct)) |>
      dplyr::mutate(dplyr::across(all_of(grpvar_fct), ~ dplyr::case_when(stat_fct == "nobs" ~ paste0("[",  ., "]"),
                                                                         stat_fct == "serr" ~ .,
                                                                         stat_fct == "mean" ~ .))) |>
      dplyr::select(-stat_fct) |>
      # Replace duplicates with NA
      dplyr::mutate(vars_fct = replace(vars_fct, duplicated(vars_fct), NA))

  }


  ttest <- main_data |>
    dplyr::filter(.data[[grpvar]] == refgrp) |>
    tidyr::nest() |>
    dplyr::rename(control_df = data) |>
    dplyr::mutate(nonref_df = purrr::map(control_df,
                                  \(x) main_data |>
                                    dplyr::filter(.data[[grpvar]] %in% nonref) |>
                                    tidyr::nest(.by = .data[[grpvar]]) |>
                                    dplyr::rename(treatment_df = data))) |>
    unnest(c(nonref_df)) |>
    dplyr::mutate(treatment_df = purrr::map2(treatment_df, .data[[grpvar]],
                                      \(d,g) d |> dplyr::mutate(!!sym(grpvar) := g))) |>
    dplyr::mutate(contrast_df = purrr::map2(control_df, treatment_df,
                                     \(x, y) dplyr::bind_rows(x, y))) |>
    dplyr::mutate(covar = purrr::map(contrast_df,
                              \(d) purrr::map(balvar, \(y) y))) |>
    # Run regressions to get difference in means (reg y D)
    dplyr::mutate(reg_diff_mean = purrr::map2(contrast_df, covar,
                                       \(d, y) fixest::feols(.[y] ~ i(.[grpvar], ref = refgrp),
                                                             fixef = fevar,
                                                             data = d,
                                                             vcov = vcov_type))) |>
    # Get data frame with mean, se, and nobs using modelsummary
    dplyr::mutate(diff_mean_list = purrr::map(reg_diff_mean,
                                       \(res) modelsummary::msummary(res,
                                                                     output = "data.frame",
                                                                     stars =  c('*' = .1, '**' = .05, '***' = 0.01),
                                                                     coef_map = paste0(grpvar, "::", nonref),
                                                                     shape = term  + model ~ statistic,
                                                                     estimate = "{estimate}{stars}",
                                                                     statistic = "{p.value}{stars}"
                                       ))) |>
    # Clean up data frame
    mutate(diff_mean_list = purrr::pmap(list(reg_diff_mean, diff_mean_list, .data[[grpvar]]),
                                 \(res, d, g) d |>
                                   {\(df) if (class(res) == "fixest")
                                     dplyr::mutate(df, model = balvar)
                                     else if (class(res) == "fixest_multi")
                                       dplyr::mutate(df, model =  stringr::str_remove(model, "lhs: "))}() |>
                                   dplyr::select(-part, -term,
                                                 vars = model, diff_mean = "Est.",
                                                 diff_mean_pval = p) |>
                                   dplyr::mutate(vars_fct = factor(vars, levels = balvar)) |>
                                   dplyr::arrange(vars_fct)|>
                                   {\(df) if (report_ttest_pval == TRUE) dplyr::select(df, -diff_mean, -vars) |>
                                       dplyr::rename_with(~ paste0(g, " vs. ", refgrp), diff_mean_pval)
                                     else dplyr::select(df, -diff_mean_pval, -vars) |>
                                       dplyr::rename_with(~ paste0(g, " vs. ", refgrp), diff_mean)}() |>
                                   dplyr::select(vars_fct, everything())
    ))


  ttest_df <- purrr::reduce(ttest$diff_mean_list, left_join, by = "vars_fct")

  ftest <- main_data |>
    dplyr::filter(.data[[grpvar]] == refgrp) |>
    tidyr::nest() |>
    dplyr::rename(control_df = data) |>
    dplyr::mutate(nonref_df = purrr::map(control_df,
                                  \(x) main_data |>
                                    dplyr::filter(.data[[grpvar]] %in% nonref) |>
                                    tidyr::nest(.by = .data[[grpvar]]) |>
                                    dplyr::rename(treatment_df = data))) |>
    tidyr::unnest(c(nonref_df)) |>
    dplyr::mutate(treatment_df = purrr::map2(treatment_df, .data[[grpvar]],
                                      \(d,g) d |> mutate(!!sym(grpvar) := g))) |>
    dplyr::mutate(contrast_df = purrr::map2(control_df, treatment_df,
                                     \(x, y) dplyr::bind_rows(x, y) |>
                                       dplyr::mutate(dvar_grpvar = if_else(.data[[grpvar]] == refgrp, 0, 1)))) |>
    # Regression
    dplyr::mutate(reg_ftest = purrr::map(contrast_df,
                                  \(d, y) fixest::feols(dvar_grpvar ~ .[balvar],
                                                        fixef = fevar,
                                                        data = d,
                                                        vcov = vcov_type))) |>
    # F-test
    dplyr::mutate(ftest_stat = purrr::map2(reg_ftest, .data[[grpvar]],
                                    \(res, g) fixest::fitstat(res, type = c("f.stat", "f.p"),
                                                              simplify = TRUE) |>
                                      as_tibble() |>
                                      dplyr::mutate(dplyr::across(everything(), ~sprintf("%.3f", round(., digits = 3)))) |>
                                      tidyr::pivot_longer(cols = everything(),
                                                          names_to = "vars_fct",
                                                          values_to = paste0(g, " vs. ", refgrp))))

  ftest_df <- purrr::reduce(ftest$ftest_stat, dplyr::left_join, by = "vars_fct")

  if (report_sd == TRUE) {
    report_df <- dplyr::full_join(mean_sd, ttest_df, by = "vars_fct") |>
      dplyr::bind_rows(ftest_df)
  } else if (report_sd == FALSE) {
    report_df <- dplyr::full_join(mean_se, ttest_df, by = "vars_fct") |>
      dplyr::bind_rows(ftest_df)
  }

  return(report_df)

}


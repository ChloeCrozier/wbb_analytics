box::use(
  here,
  dplyr,
  readr,
  tidyr,
  recipes,
  parsnip,
  workflows,
  broom,
  tibble,
  stringr,
  wehoop_stats = ./get_game_stats,
)

#' @export
get_relevant_metrics <- function(df_import) {
  relevant_games <- df_import |>
    dplyr$filter(abs(teamMargin) < 23 & abs(teamMargin) > 5) |>
    dplyr$select(-c(oppName,	gmDate,	gmSite)) # prev had gmType and teamLSc
   
  list_cols <- relevant_games |> dplyr$select(-c(teamRslt, teamMargin, teamName)) |> colnames()
  
  my_rec <- function(dat, preds) {
    recipes$recipe(dat) |>
      recipes$update_role({{preds}}, new_role = "predictor") |>
      recipes$update_role(teamMargin, new_role = "outcome")
  }

  linear_reg_model <- parsnip$linear_reg() |>
    parsnip$set_engine("glm")
  
  df <- tibble$tibble()
  
  for (j in seq_along(list_cols)) {
    norm_trans <- my_rec(relevant_games, list_cols[[j]]) |>
      recipes$step_normalize(list_cols[[j]])
  
    renewal_wflow <- workflows$workflow() |>
      workflows$add_recipe(norm_trans) |>
      workflows$add_model(linear_reg_model)
  
    renewal_wflow_fit <- parsnip$fit(renewal_wflow, data = relevant_games)
  
    df <- dplyr$bind_rows(broom$tidy(renewal_wflow_fit, number = 1), df)
  }
  
  all_metrics <- df |> dplyr$filter(term != '(Intercept)') |>
    dplyr$rename(metric = term)
  
  valuable_metrics <-  all_metrics |>
    dplyr$filter(p.value < .1) |> # Statistically significant
    dplyr$filter((stringr$str_detect(metric, "^team") & estimate > 0) | (stringr$str_detect(metric, "^opp") & estimate < 0)) # Directional correctness
  
  return(valuable_metrics)
}
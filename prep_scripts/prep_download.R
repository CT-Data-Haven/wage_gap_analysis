library(dplyr)

# match ids with json file
ids <- jsonlite::read_json("to_viz/wages-meta.json") |>
  purrr::map_chr(purrr::pluck, "id")
tbls <- readRDS("output_data/wage_gap_tbls.rds")[ids] |>
  purrr::compact() |>
  bind_rows(.id = "page") |>
  select(-dim) |>
  rename(
    universe = univ, location = name, race_ethnicity = race,
    occupation = occ_group, median_income = income
  ) |>
  mutate(universe = forcats::fct_recode(universe, all_workers = "all", full_time_only = "full")) |>
  mutate(race_ethnicity = forcats::fct_recode(race_ethnicity, Asian = "API"))

readr::write_csv(tbls, "to_viz/2022_datahaven_wage_gap.csv")
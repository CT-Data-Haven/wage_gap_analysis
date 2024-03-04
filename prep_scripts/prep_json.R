library(dplyr)
library(purrr)

# create id column for list names, then rename by index
names_to_ids <- function(x) {
  x <- purrr::imap(x, function(xi, id) {
    purrr::assign_in(xi, "id", id)
  })
  names(x) <- seq_along(x)
  x
}

ids <- list(
  total       = list(x = "name", color = NULL),
  all_by_sex  = list(x = "name", color = "sex"),
  all_by_time = list(x = "time", color = "time"),
  share_by_time = list(x = "sex", color = "time"),
  all_by_sex_x_time = list(x = "time", color = "sex"),
  full_by_sex = list(x = "sex", color = "sex"),
  full_by_edu = list(x = "edu", color = NULL),
  full_by_sex_x_edu = list(x = "edu", color = "sex"),
  full_by_race = list(x = "race", color = NULL),
  full_by_race_x_sex = list(x = "race", color = "sex"),
  full_by_sex_x_occ = list(x = "occ_group", color = "sex"),
  goal = list(x = "race", color = "sex")
)

tbls <- readRDS("output_data/wage_gap_tbls.rds") |>
  modify_at(vars(c(-total, -all_by_sex)), filter, name != "US") |>
  map(arrange, dim, name, time, edu, sex, race) |>
  map(mutate, race = forcats::fct_recode(race, Asian = "API")) |>
  map(tidyr::pivot_longer, any_of(c("income", "share")), names_to = "val_type")

tbls[["full_by_sex_x_edu"]] <- tbls[["full_by_sex_x_edu"]] |>
  filter(edu != "Total")
tbls[["full_by_race_x_sex"]] <- tbls[["full_by_race_x_sex"]] |>
  filter(race != "Total")
tbls[["full_by_race_x_sex"]] <- tbls[["full_by_race_x_sex"]] |>
  mutate(race = forcats::fct_relevel(race, "White", "Black", "Latino", "Asian")) |>
  arrange(sex, race)
tbls[["full_by_sex_x_occ"]] <- tbls[["full_by_sex_x_occ"]] |>
  mutate(occ_group = forcats::fct_reorder(occ_group, value, .fun = max, .desc = TRUE) |>
    forcats::fct_relabel(stringr::str_replace, ",? and", " &")) |>
  arrange(sex, occ_group)
tbls[["goal"]] <- tbls[["full_by_race_x_sex"]] |>
  mutate(value = max(value))
 

tbls_out <- ids |>
  imap(function(cols, id) {
    select(tbls[[id]], !!!cols, val_type, value)
  })

meta <- tbls_out |>
  map(function(x) {
    list(
      val_type = unique(x$val_type),
      direction = "vertical",
      chart_type = "clusterbar"
    )
  })
meta[["share_by_time"]][["direction"]] <- "horizontal"
meta[["share_by_time"]][["chart_type"]] <- "bar"
meta[["full_by_sex_x_occ"]][["direction"]] <- "horizontal"
meta[["full_by_sex_x_occ"]][["chart_type"]] <- "point"

meta <- names_to_ids(meta)
tbls_out <- names_to_ids(tbls_out) |>
  map(select, -val_type)

# colors: men, women, full-time, part-time
jsonlite::write_json(tbls_out, "to_viz/wages.json", auto_unbox = TRUE)
jsonlite::write_json(meta, "to_viz/meta.json", auto_unbox = TRUE)


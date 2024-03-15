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
  total = list(x = "name", color = NULL),
  all_by_sex = list(x = "name", color = "sex"),
  share_by_time = list(x = "sex", color = "time"),
  # all_by_time = list(x = "time", color = "time"),
  all_by_sex_x_time = list(x = "time", color = "sex"),
  # full_by_sex = list(x = "sex", color = "sex"),
  full_by_edu = list(x = "edu", color = NULL),
  full_by_sex_x_edu = list(x = "edu", color = "sex"),
  full_by_sex_x_occ = list(x = "occ_group", color = "sex"),
  full_by_race = list(x = "race", color = NULL),
  full_by_race_x_sex = list(x = "race", color = "sex"),
  goal = list(x = "race", color = "sex")
)

## data ----
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
    forcats::fct_relabel(stringr::str_replace, ", and", ",") |>
    forcats::fct_relabel(stringr::str_replace, "\\band\\b", "&") |>
    forcats::fct_relabel(stringr::str_replace_all, c(
      "Management" = "Mgmt",
      "Education" = "Edu.",
      "Material " = "",
      "Community Service" = "Comm'ty",
      "Natural Resources" = "Resources",
      "Maintenance" = "Maint.",
      "Transportation" = "Transport",
      "Technical" = "Tech."
    )) |>
    forcats::fct_relabel(camiller::cap_first)) |>
  arrange(sex, occ_group)
tbls[["goal"]] <- tbls[["full_by_race_x_sex"]] |>
  mutate(value = max(value))

tbls_out <- ids |>
  imap(function(cols, id) {
    tbls[[id]] |>
      select(!!!cols, val_type, value)
  }) |>
  map(function(df) {
    if ("color" %in% names(df) && all(c("Men", "Women") %in% levels(df$color))) {
      # calculate ratio
      df |>
        tidyr::pivot_wider(id_cols = c(x, val_type), names_from = color, names_glue = "{color}_{.value}") |>
        mutate(Women_ratio = Women_value / Men_value) |>
        tidyr::pivot_longer(matches("(Women|Men)"),
          names_to = c("color", ".value"),
          names_sep = "_",
          names_ptypes = list(color = factor())
        )
    } else {
      df
    } 
  })

## metadata ----
meta <- tbls_out |>
  map(function(x) {
    list(
      val_type = unique(x$val_type),
      direction = "vertical",
      chart_type = "clusterbar",
      left = 10,
      make_dumbbell = FALSE,
      use_labels = TRUE
    )
  })
# meta[["share_by_time"]][["direction"]] <- "horizontal"
meta[["share_by_time"]][["chart_type"]] <- "bar"
meta[["full_by_sex_x_occ"]][["direction"]] <- "horizontal"
meta[["full_by_sex_x_occ"]][["chart_type"]] <- "point"
meta[["full_by_sex_x_occ"]][["left"]] <- 280
meta[["full_by_sex_x_occ"]][["make_dumbbell"]] <- TRUE
meta[["full_by_sex_x_occ"]][["use_labels"]] <- FALSE
meta[["goal"]][["use_labels"]] <- FALSE

## text ----
txt <- readr::read_csv("input_data/wage_gap_text.csv") |>
  mutate(text = stringr::str_replace_all(text, "_", "\n")) |>
  mutate(id = forcats::as_factor(id)) |>
  split(~id) 

## output ----
meta <- names_to_ids(meta)
tbls_out <- names_to_ids(tbls_out) |>
  # tbls_out <- tbls_out |>
  map(select, -val_type) |>
  map(function(df) {
    if ("color" %in% names(df)) {
      df |>
        group_by(color) |>
        mutate(key = paste0(color, row_number()))
    } else {
      df |>
        group_by(x) |>
        mutate(key = paste0(x, row_number()))
    }
  }) |>
  map2(meta, function(df, mt) {
    # if clusterbar, count along x axis
    if (mt$chart_type == "clusterbar") {
      df <- df |>
        group_by(x) |>
        mutate(xoff = row_number() - 1) |>
        ungroup()
    }
    if (mt$chart_type == "bar") {
      df <- df |>
        group_by(x) |>
        mutate(cumul = cumsum(value)) |>
        ungroup()
    }
    df
  }) |>
  map(ungroup)
txt_out <- txt[names(ids)] |>
  map(select, -id) |>
  map(as.list) |>
  names_to_ids()

# colors: men, women, full-time, part-time
jsonlite::write_json(tbls_out, "to_viz/wages-data.json", auto_unbox = TRUE)
jsonlite::write_json(meta, "to_viz/wages-meta.json", auto_unbox = TRUE)
jsonlite::write_json(txt_out, "to_viz/wages-text.json", auto_unbox = TRUE)


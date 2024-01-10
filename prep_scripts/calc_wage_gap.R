library(tidyverse)
library(srvyr)
library(ipumsr)

source(here::here("_utils/misc_functions.R"))

compare_income <- function(svy_list, cols) {
  # grp_vars <- quos(...)
  svy_list |>
    map(\(x) group_by(x, name, across(any_of(cols)))) |>
    # map(group_by, name, !!!grp_vars) |>
    map_dfr(summarise,
            count = survey_total(),
            earn = survey_quantile(incearn, c(0.2, 0.25, 0.5, 0.75, 0.8), vartype = c("se")),
            sample_n = n()) |>
    mutate(across(count:sample_n, round)) |>
    ungroup()
}

hist_income <- function(svy_list, cols, brk = 1e4, max = 1e6) {
  brks <- seq(0, max, by = brk)
  svy_list |>
    map(mutate, bin = cut(incearn, breaks = c(brks, Inf), include.lowest = TRUE, right = FALSE)) |>
    map(\(x) group_by(x, name, across(any_of(cols)), bin)) |>
    map_dfr(summarise,
            count = survey_total(),
            sample_n = n()) |>
    mutate(across(count:sample_n, round)) |>
    ungroup() |>
    mutate(lower = str_extract(bin, "(?<=^[\\[\\(])(.+)(?=,)") |> parse_number()) |>
    mutate(bin = as.factor(bin) |> fct_reorder(lower))
}

# ddi <- read_ipums_ddi(here::here("fetch_data/usa_00079.xml"))
# show_pums_na(ddi)

con <- DBI::dbConnect(duckdb::duckdb(read_only = TRUE, here::here("pums.duckdb")))

DBI::dbListTables(con)

pums <- tbl(con, "wages") |>
  filter(INCEARN > 0,
         AGE >= 25) |>
  janitor::clean_names()


## ---------------------------------------------------------------------------
srvys <- list(
  us = pums |>
    as_survey_design(weights = perwt) |>
    mutate(name = "US"),
  ct = pums |>
    filter(statefip == "09") |>
    as_survey_design(weights = perwt) |>
    mutate(name = "Connecticut"),
  md = pums |>
    filter(statefip == "24") |>
    as_survey_design(weights = perwt) |>
    mutate(name = "Maryland")
)

ft_srvys <- list(
  us = pums |>
    filter(is_fulltime) |>
    as_survey_design(weights = perwt) |>
    mutate(name = "US"),
  ct = pums |>
    filter(statefip == "09") |>
    filter(is_fulltime) |>
    as_survey_design(weights = perwt) |>
    mutate(name = "Connecticut"),
  md = pums |>
    filter(statefip == "24") |>
    filter(is_fulltime) |>
    as_survey_design(weights = perwt) |>
    mutate(name = "Maryland")
)

## ---------------------------------------------------------------------------
params <- list()

params[["all_time"]] <- list(
  total = c(),
  sex = "sex",
  time = "is_fulltime",
  sex_x_time = c("sex", "is_fulltime")
)
params[["full_time"]] <- list(
  sex = "sex",
  race = "race",
  edu = "edu",
  occ = "occ_group",
  race_x_sex = c("race", "sex"),
  race_x_edu = c("race", "edu"),
  sex_x_edu = c("sex", "edu"),
  sex_x_occ = c("sex", "occ_group"),
  sex_x_edu_x_occ = c("sex", "edu", "occ_group")
)
hist_params <- list(
  sex = "sex",
  race = "race",
  race_x_sex = c("race", "sex")
)

tictoc::tic()
income_tbls <- map_dfr(params, enframe, .id = "srvy") |>
  split(~interaction(srvy, name), drop = TRUE) |>
  parallel::mclapply(function(df) {
    if (df$srvy == "all_time") {
      des <- srvys
    } else {
      des <- ft_srvys
    }
    compare_income(des, cols = df$value[[1]])
  }, mc.cores = 14)
tictoc::toc()

tictoc::tic()
income_hists <- parallel::mclapply(hist_params, function(vars) {
  des <- ft_srvys[2:3]
  hist_income(des, vars)
}, mc.cores = 14)
tictoc::toc()

saveRDS(income_tbls, file.path("input_data", "income_tbls.rds"))
saveRDS(income_hists, file.path("input_data", "income_hists.rds"))

DBI::dbDisconnect(con, shutdown = TRUE)

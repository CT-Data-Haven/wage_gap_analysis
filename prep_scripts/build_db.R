# dropping PUMAs for now since they're not in the 2022 ipums yet
# since I want to calculate nationwide values in addition to CT (and MD for class), I'm using ipumsr vignette on big data and reading pums data into a duckdb database. It's 15 million rows and some of these calculations are going to be impossible otherwise
# database is in file pums.duckdb
# also make tables of value labels to join
con <- DBI::dbConnect(duckdb::duckdb(), "pums.duckdb")

ddi <- ipumsr::read_ipums_ddi(here::here("fetch_data/usa_00081.xml"))

lookups <- list(sex = "SEX", race = "RACE", eth = "HISPAN", wks = "WKSWORK2") |>
  purrr::map(function(col) ipumsr::ipums_val_labels(ddi, col))

lookups[["edu"]] <- ipumsr::ipums_val_labels(ddi, EDUCD) |>
  dplyr::mutate(val = ifelse(val %in% c(0, 1, 999), NA, val)) |>
  dplyr::mutate(edu = cut(val, breaks = c(1, 65, 101, 114, Inf), right = FALSE,
                          labels = c("High school or less", "Some college or AA", "Bachelors", "Graduate degree")))

lookups[["occ"]] <- cwi::occ_codes |>
  dplyr::group_by(occ_group) |>
  dplyr::filter(!is_major_grp | dplyr::n() == 1) |>
  dplyr::filter(grepl("0000", soc_code)) |> # otherwise there are overlapping groups in transportation & moving
  dplyr::ungroup() |>
  tidyr::separate(occ_code, into = c("occ1", "occ2"), sep = "-") |>
  dplyr::mutate(dplyr::across(occ1:occ2, readr::parse_number),
         dplyr::across(c(occ_group, description), \(x) stringr::str_remove(x, " Occupations$"))) |>
  dplyr::select(occ_group, occ1, occ2, occ_lbl = description)

# put all lookups in db
purrr::iwalk(lookups, function(df, id) {
  # dplyr::copy_to(dest = con, df = df, name = id, overwrite = TRUE)
  DBI::dbWriteTable(conn = con, name = id, value = df, overwrite = TRUE)
})

clean_pums <- function(chunk) {
  chunk |>
    dplyr::left_join(lookups$race |> dplyr::rename(RACE = 1, race_lbl = 2), by = "RACE") |>
    dplyr::left_join(lookups$eth |> dplyr::rename(HISPAN = 1, eth_lbl = 2), by = "HISPAN") |>
    dplyr::left_join(lookups$wks |> dplyr::rename(WKSWORK2 = 1, wks_lbl = 2), by = "WKSWORK2") |>
    dplyr::left_join(lookups$edu |> dplyr::rename(EDUCD = 1, edu_lbl = 2, edu2 = 3), by = "EDUCD") |>
    dplyr::left_join(lookups$sex |> dplyr::rename(SEX = 1, sex_lbl = 2), by = "SEX") |>
    dplyr::mutate(race2 = dplyr::case_when(
      eth_lbl != "Not Hispanic" ~ "Latino",
      race_lbl == "White" ~ "White",
      race_lbl == "Black/African American" ~ "Black",
      # race_lbl == "American Indian or Alaska Native" ~ "Native", # sample size too small
      race_lbl %in% c("Japanese", "Chinese", "Other Asian or Pacific Islander") ~ "API",
      TRUE ~ "Other"
    )) |>
    dplyr::mutate(dplyr::across(c(OCC, UHRSWORK, INCEARN), \(x) dplyr::na_if(as.numeric(x), 0))) |>
    dplyr::mutate(dplyr::across(c(AGE), as.numeric)) |>
    dplyr::mutate(is_fulltime = dplyr::case_when(
      is.na(UHRSWORK) | is.na(WKSWORK2) ~ NA,
      UHRSWORK >= 35 & wks_lbl == "50-52 weeks" ~ TRUE,
      TRUE ~ FALSE
    )) |>
    # filter(INCEARN > 0,
    #        AGE >= 25) |>
    dplyr::left_join(lookups$occ,
              by = dplyr::join_by(dplyr::between(OCC, occ1, occ2))) |>
    dplyr::mutate(STATEFIP = stringr::str_pad(STATEFIP, 2, pad = "0")) |>
    # dplyr::mutate(PUMA = stringr::str_pad(as.integer(PUMA), 5, pad = "0")) |>
    # dplyr::mutate(geoid = paste0(STATEFIP, PUMA)) |>
    dplyr::select(SERIAL, MULTYEAR, 
          #  STRATA, 
           CLUSTER, STATEFIP, 
          #  geoid,
           PERNUM, PERWT, AGE, sex = sex_lbl, race = race2, edu = edu2,
           occ_group, occ_lbl, is_fulltime, INCEARN, INCEARN_CPIU_2010)
}

# from ipums vignette
ipumsr::read_ipums_micro_chunked(
  ddi = ddi,
  callback = readr::SideEffectChunkCallback$new(
    function(x, pos) {
      df <- clean_pums(x)
      if (pos == 1) {
        DBI::dbWriteTable(con, "wages", df, overwrite = TRUE)
      } else {
        DBI::dbWriteTable(con, "wages", df, row.names = FALSE, append = TRUE)
      }
    }
  ),
  chunk_size = 5e5,
  verbose = TRUE
)

DBI::dbDisconnect(con, shutdown = TRUE)

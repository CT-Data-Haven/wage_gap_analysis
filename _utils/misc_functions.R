# convenience function for converting labels to NA, such as when "99999" is used as "not in universe" for a variable that should otherwise be numeric
# ~lbl_na_if(.x, ~.val == "0")
na_lbl <- function(x, na_val = "9999999") ipumsr::lbl_na_if(x, ~.val %in% na_val)

show_pums_na <- function(ddi, pattern = "(N/A|Missing)") {
  ipumsr::ipums_var_info(ddi) %>%
    select(var_name, val_labels) %>%
    filter(!var_name %in% c("SAMPLE", "STATEFIP", "PUMA")) %>%
    tidyr::unnest(val_labels) %>%
    filter(stringr::str_detect(lbl, pattern))
}

fact2num <- function(x) readr::parse_number(as.character(x))

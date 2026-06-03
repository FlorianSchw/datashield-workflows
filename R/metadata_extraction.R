library(tidyverse)
library(here)




pattern_title <- "#' @title"
pattern_details <- "#' @details"


datashield_functions <-  as_tibble(list.files(path = "R")) |>
  mutate(package = desc::desc_get_field(key = "Package")) |>
  mutate(function_name = stringr::str_sub(value, 1, stringr::str_length(value)-2)) |>
  rename(file_name = value) |>
  rowwise() |>
  mutate(codeline_list = list(readLines(here::here("R", paste0(file_name))))) |>
  ungroup() |>
  unnest_longer(codeline_list) |>
  mutate(information_type = case_when(stringr::str_starts(pattern = pattern_title,
                                                          string = codeline_list) ~ "title",
                                      stringr::str_starts(pattern = pattern_details,
                                                          string = codeline_list) ~ "details",
                                      TRUE ~ NA_character_)) |>
  filter(!(is.na(information_type))) |>
  rowwise() |>
  mutate(information_content = case_when(stringr::str_starts(pattern = "title",
                                                             string = information_type) ~ stringr::str_replace(pattern = pattern_title,
                                                                                                               string = codeline_list,
                                                                                                               replacement = ""),
                                         stringr::str_starts(pattern = "details",
                                                             string = information_type) ~ stringr::str_replace(pattern = pattern_details,
                                                                                                               string = codeline_list,
                                                                                                               replacement = ""))) |>
  select(-codeline_list) |>
  pivot_wider(names_from = information_type,
              values_from = information_content)

jsonlite::write_json(x = datashield_functions,
                     path = "functions_metadata.json",
                     pretty = TRUE)




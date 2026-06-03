library(tidyverse)
library(here)


pattern_title <- "#' @title"
pattern_export <- "#' @export"
pattern_category <- "#' @datashield_category"
pattern_status <- "#' @datashield_status"
pattern_assign <- "DSI::datashield.assign"
pattern_aggregate <- "DSI::datashield.aggregate"

test_files <-  list.files(path = "tests/testthat")

expected_columns <- c(file_name = "",
                      package = "",
                      architecture_type = "",
                      function_name = "",
                      title = "",
                      function_type = "",
                      category = "",
                      status = "",
                      test_file = "")

datashield_functions <-  as_tibble(list.files(path = "R")) |>
  mutate(package = desc::desc_get_field(key = "Package")) |>
  mutate(function_name = stringr::str_sub(value, 1, stringr::str_length(value)-2)) |>
  rename(file_name = value) |>
  mutate(architecture_name = case_when(stringr::str_starts(pattern = "ds.",
                                                           string = function_name) ~ "client",
                                       stringr::str_ends(pattern = "DS",
                                                           string = package) ~ "server",
                                       TRUE ~ "other")) |>
  rowwise() |>
  mutate(codeline_list = list(readLines(here::here("R", paste0(file_name))))) |>
  ungroup() |>
  unnest_longer(codeline_list) |>
  mutate(information_type = case_when(stringr::str_starts(pattern = pattern_title,
                                                          string = codeline_list) ~ "title",
                                      stringr::str_starts(pattern = pattern_export,
                                                          string = codeline_list) ~ "export",
                                      stringr::str_starts(pattern = pattern_category,
                                                          string = codeline_list) ~ "category",
                                      stringr::str_starts(pattern = pattern_status,
                                                          string = codeline_list) ~ "status",
                                      TRUE ~ NA_character_)) |>
  mutate(assign_type = case_when(stringr::str_detect(pattern = pattern_assign,
                                                       string = codeline_list) ~ "assign",
                                 TRUE ~ NA_character_),
         aggregate_type = case_when(stringr::str_detect(pattern = pattern_aggregate,
                                                       string = codeline_list) ~ "aggregate",
                                   TRUE ~ NA_character_)) |>

  filter_out(is.na(information_type) & is.na(assign_type) & is.na(aggregate_type)) |>
  mutate(information_type = case_when(is.na(information_type) & is.na(assign_type) ~ "aggregate",
                                      is.na(information_type) & is.na(aggregate_type) ~ "assign",
                                      TRUE ~ information_type)) |>
  select(-c(assign_type, aggregate_type)) |>
  rowwise() |>
  mutate(information_content = case_when(information_type == "title" ~ stringr::str_replace(pattern = pattern_title,
                                                                                                               string = codeline_list,
                                                                                                               replacement = ""),
                                         information_type == "category" ~ stringr::str_replace(pattern = pattern_category,
                                                                                                               string = codeline_list,
                                                                                                               replacement = ""),
                                         information_type == "status" ~ stringr::str_replace(pattern = pattern_status,
                                                                                                               string = codeline_list,
                                                                                                               replacement = ""),
                                         information_type == "export" ~ "export",
                                         information_type == "assign" ~ "assign",
                                         information_type == "aggregate" ~ "aggregate")) |>
  select(-codeline_list) |>
  pivot_wider(names_from = information_type,
              values_from = information_content) |>
  mutate(function_type = case_when(assign == "assign" & aggregate == "aggregate" ~ "hybrid",
                                   assign == "assign" ~ "assign",
                                   aggregate == "aggregate" ~ "aggregate",
                                   TRUE ~ "other"),
         architecture_type = case_when(architecture_name == "client" & export == "export" ~ "client",
                                       architecture_name == "client" & is.na(export) ~ "client (no export)",
                                       architecture_name == "server" & export == "export" ~ "server",
                                       architecture_name == "server" & is.na(export) ~ "server (no export)",
                                       architecture_name == "other" & export == "export" ~ "other",
                                       TRUE ~ NA_character_)) |>
  filter(!(is.na(architecture_type))) |>
  rowwise() |>
  mutate(test_file = stringr::str_detect(pattern = function_name,
                                         string = paste(test_files,collapse = ", "))) |>
  select(-c(assign, aggregate,  architecture_name))

datashield_functions <- datashield_functions |>
  add_column(!!!expected_columns[!names(expected_columns) %in% colnames(datashield_functions)]) |>
  mutate(across(everything(), ~ifelse(is.na(.x), "", .x))) |>
  select(c(package, function_name, architecture_type, function_type, title, category, status, test_file))

jsonlite::write_json(x = datashield_functions,
                     path = paste0(desc::desc_get_field(key = "Package"),"_functions_metadata.json"),
                     pretty = TRUE)




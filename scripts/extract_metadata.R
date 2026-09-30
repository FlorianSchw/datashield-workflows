#!/usr/bin/env Rscript
# Extracts function-level metadata from the R files of the DataSHIELD packages
# listed in package_list.csv and compiles them into one JSON file.
#
# Run from the repository root: Rscript scripts/extract_metadata.R
# Which metadata is extracted is defined in config/metadata_fields.yml.

library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

walk(list.files("R/utils", pattern = "\\.R$", full.names = TRUE), source)

config <- yaml::read_yaml("config/metadata_fields.yml")
arch <- config$architecture
fields <- config$fields |> set_names(map_chr(config$fields, "name"))
value_fields <- names(keep(fields, \(f) f$extract == "value"))


datashield_functions <- read.delim("package_list.csv", sep = "|", strip.white = TRUE,
                                   na.strings = "") |>
  filter(!is.na(github_link)) |>
  select(name, github_link) |>
  pmap(\(name, github_link) tryCatch(extract_package(name, github_link, fields),
                                     error = \(e) {
                                       warning("Failed on ", name, ": ", conditionMessage(e))
                                       NULL
                                     })) |>
  list_rbind() |>
  mutate(architecture_name = case_when(str_detect(function_name, arch$client_function_pattern) ~ "client",
                                       str_detect(package, arch$server_package_pattern) ~ "server",
                                       TRUE ~ "other"),
         architecture_type = case_when(exported ~ architecture_name,
                                       architecture_name != "other" ~ str_c(architecture_name, " (no export)"),
                                       TRUE ~ NA_character_),
         function_type = case_when(calls_assign & calls_aggregate ~ "hybrid",
                                   calls_assign ~ "assign",
                                   calls_aggregate ~ "aggregate",
                                   TRUE ~ "other")) |>
  filter(!is.na(architecture_type)) |>
  arrange(package, function_name) |>
  select(package, function_name, architecture_type, function_type, all_of(value_fields), test_file)

dir.create(dirname(config$output$file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(datashield_functions, config$output$file, pretty = TRUE)
message("Wrote ", nrow(datashield_functions), " functions to ", config$output$file)

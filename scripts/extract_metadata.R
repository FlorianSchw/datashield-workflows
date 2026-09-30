#!/usr/bin/env Rscript
# Extracts function-level metadata from the R files of the DataSHIELD packages
# listed in package_list.csv and compiles them into one JSON file.
#
# Run from the repository root: Rscript scripts/extract_metadata.R
# Which metadata is extracted is defined in config/metadata_fields.yml.

purrr::walk(list.files("R/utils", pattern = "\\.R$", full.names = TRUE), source)

config <- yaml::read_yaml("config/metadata_fields.yml")
arch <- config$architecture
fields <- config$fields |> purrr::set_names(purrr::map_chr(config$fields, "name"))
value_fields <- names(purrr::keep(fields, \(f) f$extract == "value"))

datashield_functions <- read.delim("package_list.csv", sep = "|", strip.white = TRUE,
                                   na.strings = "") |>
  dplyr::filter(!is.na(github_link)) |>
  dplyr::select(name, github_link) |>
  purrr::pmap(\(name, github_link) tryCatch(extract_package(name, github_link, fields),
                                            error = \(e) {
                                              warning("Failed on ", name, ": ", conditionMessage(e))
                                              NULL
                                            })) |>
  purrr::list_rbind() |>
  dplyr::mutate(architecture_name = dplyr::case_when(stringr::str_detect(function_name, arch$client_function_pattern) ~ "client",
                                                     stringr::str_detect(package, arch$server_package_pattern) ~ "server",
                                                     TRUE ~ "other"),
                architecture_type = dplyr::case_when(exported ~ architecture_name,
                                                     architecture_name != "other" ~ stringr::str_c(architecture_name, " (no export)"),
                                                     TRUE ~ NA_character_),
                function_type = dplyr::case_when(calls_assign & calls_aggregate ~ "hybrid",
                                                 calls_assign ~ "assign",
                                                 calls_aggregate ~ "aggregate",
                                                 TRUE ~ "other")) |>
  dplyr::filter(!is.na(architecture_type)) |>
  dplyr::arrange(package, function_name) |>
  dplyr::select(package, function_name, architecture_type, function_type,
                dplyr::all_of(value_fields), test_file)

dir.create(dirname(config$output$file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(datashield_functions, config$output$file, pretty = TRUE)
message("Wrote ", nrow(datashield_functions), " functions to ", config$output$file)

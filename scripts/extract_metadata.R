#!/usr/bin/env Rscript
# Extracts function-level metadata from the R files of the DataSHIELD packages
# listed in package_list.csv and compiles them into one JSON file.
#
# Usage: Rscript scripts/extract_metadata.R [package_list.csv] [config.yml]
# Which metadata is extracted is defined in config/metadata_fields.yml.

library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
package_list_path <- if (length(args) >= 1) args[[1]] else "package_list.csv"
config_path <- if (length(args) >= 2) args[[2]] else "config/metadata_fields.yml"

config <- yaml::read_yaml(config_path)
arch <- config$architecture
fields <- config$fields |> set_names(map_chr(config$fields, "name"))
value_fields <- names(keep(fields, \(f) f$extract == "value"))

pattern_roxygen <- "^\\s*#'"
pattern_any_tag <- "^\\s*#'\\s*@"


# Text after the first match of `pattern`, including the following roxygen
# lines until the next tag or the end of the roxygen block.
extract_value <- function(lines, pattern) {
  start <- str_which(lines, pattern)[1]
  if (is.na(start)) return("")

  following <- lines[-seq_len(start)]
  block_end <- which(!str_detect(following, pattern_roxygen) |
                       str_detect(following, pattern_any_tag))[1]
  continuation <- head(following, coalesce(block_end - 1L, length(following)))

  c(str_remove(lines[start], pattern), str_remove(continuation, pattern_roxygen)) |>
    str_c(collapse = " ") |>
    str_squish()
}

extract_fields <- function(file) {
  lines <- readLines(file, warn = FALSE)
  map(fields, \(f) switch(f$extract,
                          flag = any(str_detect(lines, f$pattern)),
                          value = extract_value(lines, f$pattern)))
}

extract_package <- function(name, github_link) {
  message("Extracting ", name)
  repo <- file.path(tempdir(), name)
  if (system2("git", c("clone", "--quiet", "--depth", "1", github_link, repo)) != 0) {
    warning("Could not clone ", github_link, ", skipping ", name)
    return(NULL)
  }
  test_files <- list.files(file.path(repo, "tests", "testthat")) |> str_c(collapse = ", ")

  tibble(file = list.files(file.path(repo, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) |>
    mutate(package = read.dcf(file.path(repo, "DESCRIPTION"), fields = "Package")[1, 1],
           function_name = str_remove(basename(file), "\\.[Rr]$"),
           test_file = str_detect(test_files, fixed(function_name)),
           metadata = map(file, extract_fields)) |>
    unnest_wider(metadata)
}


datashield_functions <- read.delim(package_list_path, sep = "|", strip.white = TRUE,
                                   na.strings = "") |>
  filter(!is.na(github_link)) |>
  select(name, github_link) |>
  pmap(\(name, github_link) tryCatch(extract_package(name, github_link),
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

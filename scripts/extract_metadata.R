#!/usr/bin/env Rscript
# Extracts function-level metadata from the R files of the DataSHIELD packages
# listed in package_list.csv and compiles them into one JSON file.
#
# Usage: Rscript scripts/extract_metadata.R [package_list.csv] [config.yml]
# Which metadata is extracted is defined in config/metadata_fields.yml.

args <- commandArgs(trailingOnly = TRUE)
package_list_path <- if (length(args) >= 1) args[[1]] else "package_list.csv"
config_path <- if (length(args) >= 2) args[[2]] else "config/metadata_fields.yml"

config <- yaml::read_yaml(config_path)

# --- package list ------------------------------------------------------------

read_package_list <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  sep <- if (grepl("|", first_line, fixed = TRUE)) "|" else ","
  packages <- utils::read.delim(path, sep = sep, quote = "\"", strip.white = TRUE,
                                stringsAsFactors = FALSE, na.strings = "")
  packages[!is.na(packages$github_link), , drop = FALSE]
}

# --- helpers -----------------------------------------------------------------

clone_repo <- function(url, dest) {
  status <- suppressWarnings(system2("git",
                                     c("clone", "--quiet", "--depth", "1", url, dest),
                                     stdout = FALSE, stderr = FALSE))
  status == 0
}

extract_field <- function(lines, field) {
  hits <- grepl(field$pattern, lines, perl = TRUE)
  if (identical(field$extract, "flag")) {
    return(any(hits))
  }
  if (!any(hits)) {
    return("")
  }
  trimws(sub(field$pattern, "", lines[which(hits)[1]], perl = TRUE))
}

extract_file <- function(file, package, test_files) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  function_name <- sub("\\.[Rr]$", "", basename(file))
  values <- lapply(config$fields, extract_field, lines = lines)
  names(values) <- vapply(config$fields, `[[`, character(1), "name")

  arch <- config$architecture
  base_type <- if (grepl(arch$client_function_pattern, function_name, perl = TRUE)) {
    "client"
  } else if (grepl(arch$server_package_pattern, package, perl = TRUE)) {
    "server"
  } else {
    "other"
  }
  exported <- isTRUE(values$exported)
  if (base_type == "other" && !exported) {
    return(NULL)
  }
  architecture_type <- if (exported || base_type == "other") base_type else
    paste(base_type, "(no export)")

  function_type <- if (isTRUE(values$calls_assign) && isTRUE(values$calls_aggregate)) {
    "hybrid"
  } else if (isTRUE(values$calls_assign)) {
    "assign"
  } else if (isTRUE(values$calls_aggregate)) {
    "aggregate"
  } else {
    "other"
  }

  # fields that only feed the derived columns are not written to the output
  helper <- c("exported", "calls_assign", "calls_aggregate")
  c(list(package = package,
         function_name = function_name,
         architecture_type = architecture_type,
         function_type = function_type),
    values[setdiff(names(values), helper)],
    list(test_file = any(grepl(function_name, test_files, fixed = TRUE))))
}

extract_package <- function(pkg) {
  dir <- file.path(tempdir(), paste0("repo_", pkg$name))
  if (!clone_repo(pkg$github_link, dir)) {
    warning("Could not clone ", pkg$github_link, ", skipping ", pkg$name)
    return(list())
  }
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  desc_file <- file.path(dir, "DESCRIPTION")
  package <- if (file.exists(desc_file)) {
    read.dcf(desc_file, fields = "Package")[1, 1]
  } else {
    pkg$name
  }
  r_files <- list.files(file.path(dir, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  test_files <- list.files(file.path(dir, "tests", "testthat"))

  Filter(Negate(is.null), lapply(r_files, extract_file,
                                 package = package, test_files = test_files))
}

# --- main --------------------------------------------------------------------

packages <- read_package_list(package_list_path)
results <- list()
for (i in seq_len(nrow(packages))) {
  pkg <- packages[i, ]
  message("Extracting ", pkg$name)
  results <- c(results, tryCatch(extract_package(pkg), error = function(e) {
    warning("Failed on ", pkg$name, ": ", conditionMessage(e))
    list()
  }))
}

# deterministic order keeps commits free of noise
ord <- order(vapply(results, `[[`, "", "package"), vapply(results, `[[`, "", "function_name"))
results <- results[ord]

dir.create(dirname(config$output$file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(results, config$output$file, pretty = TRUE, auto_unbox = TRUE)
message("Wrote ", length(results), " functions from ", nrow(packages), " packages to ",
        config$output$file)

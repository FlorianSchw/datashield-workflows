# Clones a package repository and extracts the fields from each of its R files.
extract_package <- function(name, github_link, fields) {
  message("Extracting ", name)
  repo <- file.path(tempdir(), name)
  if (system2("git", c("clone", "--quiet", "--depth", "1", github_link, repo)) != 0) {
    warning("Could not clone ", github_link, ", skipping ", name)
    return(NULL)
  }
  test_files <- list.files(file.path(repo, "tests", "testthat")) |> stringr::str_c(collapse = ", ")

  package_functions <- dplyr::tibble(file = list.files(file.path(repo, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) |>
    dplyr::mutate(package = read.dcf(file.path(repo, "DESCRIPTION"), fields = "Package")[1, 1],
                  function_name = stringr::str_remove(basename(file), "\\.[Rr]$"),
                  test_file = stringr::str_detect(test_files, stringr::fixed(function_name)),
                  metadata = purrr::map(file, extract_fields, fields = fields)) |>
    tidyr::unnest_wider(metadata)

  return(package_functions)
}

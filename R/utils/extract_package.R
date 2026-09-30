# Clones a package repository and extracts the fields from each of its R files.
extract_package <- function(name, github_link, fields) {
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
           metadata = map(file, extract_fields, fields = fields)) |>
    unnest_wider(metadata)
}

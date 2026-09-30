# Text after the first match of `pattern`, including the following roxygen
# lines until the next tag or the end of the roxygen block.
extract_value <- function(lines, pattern) {
  pattern_roxygen <- "^\\s*#'"
  pattern_any_tag <- "^\\s*#'\\s*@"

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

# Applies all configured fields to one R file.
extract_fields <- function(file, fields) {
  lines <- readLines(file, warn = FALSE)
  map(fields, \(f) switch(f$extract,
                          flag = any(str_detect(lines, f$pattern)),
                          value = extract_value(lines, f$pattern)))
}

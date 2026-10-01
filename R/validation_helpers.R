suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readxl)
  library(readr)
  library(tibble)
  library(janitor)
  library(digest)
  library(jsonlite)
  library(writexl)
  library(cli)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

normalize_minus <- function(x) {
  x |>
    as.character() |>
    str_replace_all("\u2212|\u2013|\u2014", "-") |>
    str_replace_all("\u00A0", " ") |>
    str_trim()
}

parse_numeric_exact <- function(x) {
  x <- normalize_minus(x)
  suppressWarnings(as.numeric(str_replace_all(x, ",", "")))
}

parse_ci_pair <- function(x) {
  x <- normalize_minus(x)
  x[x %in% c("", "NR", "NA", "N/A")] <- NA_character_
  x <- str_replace_all(x, "%|,", "")
  pattern <- "^\\s*(?:range\\s*)?(-?\\d+(?:\\.\\d+)?)\\s*(?:-|to)\\s*(-?\\d+(?:\\.\\d+)?)\\s*$"
  m <- str_match(x, regex(pattern, ignore_case = TRUE))
  tibble(ci_lower = suppressWarnings(as.numeric(m[,2])),
         ci_upper = suppressWarnings(as.numeric(m[,3])))
}

read_sheet_strict <- function(path, sheet, required_cols = NULL) {
  available <- excel_sheets(path)
  if (!sheet %in% available) {
    cli_abort("Required sheet {.val {sheet}} is absent from {.file {path}}.")
  }
  out <- read_excel(path, sheet = sheet, .name_repair = "minimal") |>
    mutate(across(everything(), ~ if (is.character(.x)) str_squish(.x) else .x))
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(out))
    if (length(missing)) {
      cli_abort("Sheet {.val {sheet}} is missing columns: {paste(missing, collapse=', ')}")
    }
  }
  out
}

new_issue_log <- function() {
  tibble(
    check_id = character(), level = character(), status = character(),
    finding = character(), n_affected = integer(), affected_ids = character(),
    required_action = character()
  )
}

append_issue <- function(log, check_id, level, status, finding,
                         affected_ids = character(), required_action = "") {
  bind_rows(log, tibble(
    check_id = check_id,
    level = level,
    status = status,
    finding = finding,
    n_affected = length(affected_ids),
    affected_ids = paste(affected_ids, collapse = ", "),
    required_action = required_action
  ))
}

assert_unique_nonmissing <- function(data, column, pattern = NULL) {
  values <- data[[column]]
  dup <- values[duplicated(values) & !is.na(values) & values != ""]
  missing <- which(is.na(values) | values == "")
  invalid <- integer()
  if (!is.null(pattern)) {
    invalid <- which(!is.na(values) & values != "" & !str_detect(values, pattern))
  }
  list(
    ok = length(dup) == 0L && length(missing) == 0L && length(invalid) == 0L,
    duplicates = unique(dup), missing_rows = missing, invalid_rows = invalid
  )
}

cca_standard <- function(N, r, c) {
  denominator <- r * c - r
  ifelse(denominator == 0, NA_real_, (N - r) / denominator)
}

write_validation_outputs <- function(objects, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  walk2(objects, names(objects), function(obj, nm) {
    saveRDS(obj, file.path(out_dir, paste0(nm, ".rds")))
    if (is.data.frame(obj)) {
      write_csv(obj, file.path(out_dir, paste0(nm, ".csv")), na = "")
    }
  })
}

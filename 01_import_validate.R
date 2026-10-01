source("00_environment.R")
source("R/validation_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
master_path <- if (length(args) >= 1L) args[[1]] else Sys.getenv(
  "MOY_MASTER", "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx"
)
sap_path <- if (length(args) >= 2L) args[[2]] else Sys.getenv(
  "MOY_SAP", "data/moyamoya_umbrella_SAP_v1_analysis_specification.xlsx"
)

if (!file.exists(master_path)) cli_abort("Master workbook not found: {.file {master_path}}")
if (!file.exists(sap_path)) cli_abort("SAP workbook not found: {.file {sap_path}}")

config <- fromJSON("config/validation_config.json", simplifyVector = FALSE)
issues <- new_issue_log()

master_sheets <- excel_sheets(master_path)
missing_sheets <- setdiff(unlist(config$required_master_sheets), master_sheets)
issues <- append_issue(
  issues, "S01", "Fatal", ifelse(length(missing_sheets) == 0, "PASS", "FAIL"),
  ifelse(length(missing_sheets) == 0,
         "All required master sheets are present.",
         paste("Missing sheets:", paste(missing_sheets, collapse = ", "))),
  missing_sheets, "Restore missing sheets before analysis."
)

review_master <- read_sheet_strict(
  master_path, "Review master",
  c("Review ID","Screening ID","Master tier","Formal umbrella unit","Title",
    "Master AMSTAR 2 overall","Master ROBIS overall")
)
effects_raw <- read_sheet_strict(
  master_path, "Analysis-ready effects",
  c("Effect ID","Review ID","Screening ID","Master tier","Review title",
    "Effect measure","Estimate","95% CI","Exact PDF page",
    "Master AMSTAR 2","Master ROBIS","Final analysis disposition")
)
amstar_summary <- read_sheet_strict(
  master_path, "AMSTAR 2 summary", c("Review ID","Master overall")
)
robis_summary <- read_sheet_strict(
  master_path, "ROBIS summary", c("Review ID","Master overall")
)
amstar_items <- read_sheet_strict(master_path, "AMSTAR 2 items", c("Review ID","Item"))
robis_items <- read_sheet_strict(master_path, "ROBIS items", c("Review ID","Domain"))
primary_publications <- read_sheet_strict(
  master_path, "Primary publications", c("Primary publication ID")
)
publication_occurrences <- read_sheet_strict(
  master_path, "Publication occurrences",
  c("Primary publication ID","Review ID")
)
overlap_matrix <- read_sheet_strict(
  master_path, "Publication overlap matrix", c("Primary publication ID")
)
cca_table <- read_sheet_strict(
  master_path, "CCA adjusted",
  c("Scope","Reviews contributing publications","Adjusted unique primary publications",
    "Adjusted total occurrences","CCA")
)
missing_reports <- read_sheet_strict(master_path, "Missing full texts")

# Hashes and source manifest
file_manifest <- tibble(
  role = c("Frozen master", "SAP specification"),
  path = c(normalizePath(master_path), normalizePath(sap_path)),
  sha256 = c(digest(file = master_path, algo = "sha256"),
             digest(file = sap_path, algo = "sha256")),
  imported_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
)

# Row-count checks
issues <- append_issue(
  issues, "S02", "Fatal",
  ifelse(nrow(effects_raw) == config$expected$raw_analysis_ready_effects, "PASS", "FAIL"),
  sprintf("Analysis-ready effects contains %d rows; expected %d.",
          nrow(effects_raw), config$expected$raw_analysis_ready_effects),
  if (nrow(effects_raw) == config$expected$raw_analysis_ready_effects) character()
  else as.character(nrow(effects_raw)),
  "Reconcile row count."
)
issues <- append_issue(
  issues, "S03", "Fatal",
  ifelse(nrow(review_master) == config$expected$review_units, "PASS", "FAIL"),
  sprintf("Review master contains %d units; expected %d.",
          nrow(review_master), config$expected$review_units),
  character(), "Reconcile review inventory."
)

# ID integrity
effect_id_check <- assert_unique_nonmissing(effects_raw, "Effect ID", "^E\\d{4}$")
review_id_check <- assert_unique_nonmissing(review_master, "Review ID", "^R\\d{3}$")
issues <- append_issue(
  issues, "V01a", "Fatal", ifelse(effect_id_check$ok, "PASS", "FAIL"),
  "Effect IDs are unique, nonmissing and conform to E####.",
  c(effect_id_check$duplicates, paste0("row:", effect_id_check$missing_rows),
    paste0("row:", effect_id_check$invalid_rows)),
  "Correct effect identifiers."
)
issues <- append_issue(
  issues, "V01b", "Fatal", ifelse(review_id_check$ok, "PASS", "FAIL"),
  "Review IDs are unique, nonmissing and conform to R###.",
  c(review_id_check$duplicates, paste0("row:", review_id_check$missing_rows),
    paste0("row:", review_id_check$invalid_rows)),
  "Correct review identifiers."
)

formal_reviews <- review_master |> filter(`Formal umbrella unit` == "Yes")
formal_screening <- formal_reviews |>
  filter(is.na(`Screening ID`) | !str_detect(`Screening ID`, "^MOY-\\d{4}$") |
           duplicated(`Screening ID`))
issues <- append_issue(
  issues, "V01c", "Fatal", ifelse(nrow(formal_screening) == 0, "PASS", "FAIL"),
  "Every formal review has one valid and unique MOY-#### screening identifier.",
  formal_screening$`Review ID`, "Correct formal-review screening mappings."
)

# Strict joins
joined <- effects_raw |>
  left_join(
    review_master |>
      select(`Review ID`, review_screening = `Screening ID`,
             review_tier = `Master tier`, review_title = Title,
             formal_unit = `Formal umbrella unit`,
             review_amstar = `Master AMSTAR 2 overall`,
             review_robis = `Master ROBIS overall`),
    by = "Review ID"
  )

join_mismatch <- joined |>
  filter(
    is.na(review_title) |
      `Screening ID` != review_screening |
      `Master tier` != review_tier |
      `Review title` != review_title |
      `Master AMSTAR 2` != review_amstar |
      `Master ROBIS` != review_robis
  )
issues <- append_issue(
  issues, "V01d", "Fatal", ifelse(nrow(join_mismatch) == 0, "PASS", "FAIL"),
  "Effect-review joins, titles, tiers, screening IDs and quality judgments are consistent.",
  join_mismatch$`Effect ID`, "Repair joins before analysis."
)

# V02: quarantine nonformal rows without mutating the frozen workbook.
quarantine <- joined |> filter(formal_unit != "Yes")
effects_formal <- joined |> filter(formal_unit == "Yes")

expected_quarantine <- sort(unlist(config$expected$quarantined_tier3_effects))
actual_quarantine <- sort(quarantine$`Effect ID`)
quarantine_exact <- identical(expected_quarantine, actual_quarantine)

issues <- append_issue(
  issues, "V02", "Fatal", ifelse(nrow(quarantine) == 0, "PASS", "FAIL"),
  sprintf("%d raw analysis-ready effects originate from nonformal Tier 3 reviews.",
          nrow(quarantine)),
  quarantine$`Effect ID`,
  "Quarantine from formal analysis; retain in context-only/evidence-gap outputs."
)
issues <- append_issue(
  issues, "V02_post", "Fatal",
  ifelse(nrow(effects_formal) == config$expected$formal_effects_after_quarantine &&
           quarantine_exact, "PASS", "FAIL"),
  sprintf("After deterministic quarantine, %d formal effects remain and the five expected Tier 3 rows are isolated.",
          nrow(effects_formal)),
  if (quarantine_exact) character() else setdiff(actual_quarantine, expected_quarantine),
  "Stop if the quarantine set differs from the amendment."
)

# Source locators
page_pattern <- "\\[MESCLADO\\s+[1-4]\\s+pp?\\.\\s*\\d+(?:\\s*[–-]\\s*\\d+)?(?:,\\s*\\d+)?\\]"
bad_pages <- joined |> filter(!str_detect(`Exact PDF page`, regex(page_pattern, ignore_case = TRUE)))
issues <- append_issue(
  issues, "V14", "Moderate", ifelse(nrow(bad_pages) == 0, "PASS", "FAIL"),
  "Every effect has an exact MESCLADO page or page-range locator.",
  bad_pages$`Effect ID`, "Trace source pages."
)

# Quantitative parsing
ci <- parse_ci_pair(effects_formal$`95% CI`)
effects_formal <- effects_formal |>
  mutate(
    estimate_numeric = parse_numeric_exact(Estimate),
    ci_lower = ci$ci_lower,
    ci_upper = ci$ci_upper,
    comparative_measure = `Effect measure` %in%
      c(unlist(config$ratio_measures), unlist(config$continuous_comparative_measures)),
    forest_ready_direct = comparative_measure & !is.na(estimate_numeric) &
      !is.na(ci_lower) & !is.na(ci_upper),
    forest_missing_ci = comparative_measure & !is.na(estimate_numeric) &
      (is.na(ci_lower) | is.na(ci_upper)),
    ci_contains_estimate = if_else(
      forest_ready_direct,
      ci_lower <= estimate_numeric & estimate_numeric <= ci_upper,
      NA
    )
  )

forest_missing <- effects_formal |> filter(forest_missing_ci)
invalid_ci <- effects_formal |> filter(forest_ready_direct & !ci_contains_estimate)
invalid_ratio <- effects_formal |>
  filter(`Effect measure` %in% unlist(config$ratio_measures),
         (!is.na(estimate_numeric) & estimate_numeric <= 0) |
           (!is.na(ci_lower) & ci_lower < 0) |
           (!is.na(ci_upper) & ci_upper <= 0))

issues <- append_issue(
  issues, "V03", "Major", ifelse(nrow(forest_missing) == 0, "PASS", "WARNING"),
  sprintf("%d formal comparative rows are directly forest-ready; %d numeric estimates lack a usable CI.",
          sum(effects_formal$forest_ready_direct), nrow(forest_missing)),
  forest_missing$`Effect ID`,
  "Exclude from precision/forest analyses unless a deterministic CI derivation is documented."
)
issues <- append_issue(
  issues, "V04", "Fatal", ifelse(nrow(invalid_ci) == 0, "PASS", "FAIL"),
  "Every parseable comparative point estimate lies within its confidence interval.",
  invalid_ci$`Effect ID`, "Source recheck."
)
issues <- append_issue(
  issues, "V05", "Fatal", ifelse(nrow(invalid_ratio) == 0, "PASS", "FAIL"),
  "All parseable OR/RR/HR values and confidence limits are positive.",
  invalid_ratio$`Effect ID`, "Source recheck."
)

# Quality completeness
amstar_counts <- amstar_items |> count(`Review ID`, name = "n_items")
robis_counts <- robis_items |> count(`Review ID`, name = "n_rows")
bad_amstar <- review_master |>
  select(`Review ID`) |>
  left_join(amstar_counts, by = "Review ID") |>
  filter(is.na(n_items) | n_items != config$expected$amstar_items_per_review)
bad_robis <- review_master |>
  select(`Review ID`) |>
  left_join(robis_counts, by = "Review ID") |>
  filter(is.na(n_rows) | n_rows != config$expected$robis_rows_per_review)

issues <- append_issue(
  issues, "Q01", "Major", ifelse(nrow(bad_amstar) == 0, "PASS", "FAIL"),
  "Every review unit has 16 AMSTAR 2 item judgments.",
  bad_amstar$`Review ID`, "Complete AMSTAR 2."
)
issues <- append_issue(
  issues, "Q02", "Major", ifelse(nrow(bad_robis) == 0, "PASS", "FAIL"),
  "Every review unit has five ROBIS rows.",
  bad_robis$`Review ID`, "Complete ROBIS."
)

# Overlap integrity
occ_clean <- publication_occurrences |> filter(`Primary publication ID` != "REMOVED")
matrix_long <- overlap_matrix |>
  pivot_longer(cols = matches("^R\\d{3}$"), names_to = "Review ID", values_to = "included") |>
  filter(toupper(as.character(included)) %in% c("X","1","TRUE","YES")) |>
  select(`Primary publication ID`, `Review ID`)

occ_pairs <- occ_clean |> distinct(`Primary publication ID`, `Review ID`)
matrix_pairs <- matrix_long |> distinct(`Primary publication ID`, `Review ID`)
overlap_diff <- bind_rows(
  anti_join(occ_pairs, matrix_pairs, by = c("Primary publication ID","Review ID")) |>
    mutate(source = "occurrence_only"),
  anti_join(matrix_pairs, occ_pairs, by = c("Primary publication ID","Review ID")) |>
    mutate(source = "matrix_only")
)
issues <- append_issue(
  issues, "O02", "Fatal", ifelse(nrow(overlap_diff) == 0, "PASS", "FAIL"),
  "Publication occurrences and the overlap matrix agree after removed false keys are excluded.",
  paste(overlap_diff$`Primary publication ID`, overlap_diff$`Review ID`, sep = "|"),
  "Repair overlap objects."
)

cca_check <- cca_table |>
  transmute(
    Scope,
    reviews_c = as.numeric(`Reviews contributing publications`),
    unique_publications_r = as.numeric(`Adjusted unique primary publications`),
    occurrences_N = as.numeric(`Adjusted total occurrences`),
    reported_CCA = as.numeric(CCA),
    recalculated_CCA = cca_standard(occurrences_N, unique_publications_r, reviews_c),
    absolute_difference = abs(reported_CCA - recalculated_CCA),
    status = if_else(absolute_difference < 1e-12, "PASS", "FAIL")
  )
issues <- append_issue(
  issues, "O03", "Major", ifelse(all(cca_check$status == "PASS"), "PASS", "FAIL"),
  "CCA values reproduce the standard formula from adjusted publication counts.",
  cca_check$Scope[cca_check$status == "FAIL"], "Correct CCA."
)

# Heterogeneous raw labels are an expected input to 02_harmonize_cells.R.
issues <- append_issue(
  issues, "H01", "Major", "EXPECTED",
  sprintf("Raw effects contain %d measure labels, %d horizon labels and %d unit labels.",
          n_distinct(effects_raw$`Effect measure`),
          n_distinct(effects_raw$Horizon),
          n_distinct(effects_raw$Unit)),
  character(), "Apply SAP registries before inferential analysis."
)

# Final post-quarantine status.
unresolved_fatal <- issues |> filter(level == "Fatal", status == "FAIL", check_id != "V02")
post_status <- if (nrow(unresolved_fatal) == 0) {
  "PASS WITH DOCUMENTED QUARANTINE AND NONFATAL FOREST-READINESS WARNING"
} else {
  "FAIL"
}

summary <- list(
  validation_version = "1.0",
  master_sha256 = file_manifest$sha256[[1]],
  sap_sha256 = file_manifest$sha256[[2]],
  review_units = nrow(review_master),
  formal_reviews = nrow(formal_reviews),
  raw_effects = nrow(effects_raw),
  formal_effects = nrow(effects_formal),
  quarantined_context_effects = nrow(quarantine),
  direct_forest_ready = sum(effects_formal$forest_ready_direct),
  comparative_numeric_without_ci = nrow(forest_missing),
  pre_quarantine_status = "FAIL — V02",
  post_quarantine_status = post_status
)

outputs <- list(
  review_master_validated = review_master,
  effects_raw_imported = effects_raw,
  effects_formal_validated = effects_formal,
  effects_context_quarantined = quarantine,
  forest_not_ready_no_ci = forest_missing,
  validation_findings = issues,
  file_manifest = file_manifest,
  cca_verification = cca_check,
  overlap_difference = overlap_diff
)
write_validation_outputs(outputs, "derived")
write_json(summary, "derived/validation_summary.json", pretty = TRUE, auto_unbox = TRUE)
write_xlsx(outputs, "derived/moyamoya_import_validation_report_R.xlsx")

fatal_after_quarantine <- issues |>
  filter(level == "Fatal", status == "FAIL", check_id != "V02")
if (nrow(fatal_after_quarantine) > 0) {
  print(fatal_after_quarantine)
  cli_abort("Validation failed after quarantine. See derived/validation_findings.csv.")
}

message("Validation completed.")
message("Formal effects available for harmonization: ", nrow(effects_formal))
message("Quarantined Tier 3 effects: ", nrow(quarantine))
message("Post-quarantine status: ", post_status)

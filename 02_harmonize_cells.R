source("00_environment.R")
source("R/validation_helpers.R")
source("R/harmonization_helpers.R")

# Run module 01 automatically when the validated derivative does not yet exist.
if (!file.exists("derived/effects_formal_validated.rds")) {
  message("Validated derivative not found. Running 01_import_validate.R first.")
  source("01_import_validate.R")
}

config <- fromJSON("config/harmonization_config.json", simplifyVector = FALSE)
effects_formal <- readRDS("derived/effects_formal_validated.rds")
registry <- read_locked_registry()

issues <- new_issue_log()

# H01: registry integrity and exact coverage.
registry_id_check <- assert_unique_nonmissing(registry, "Effect ID", "^E\\d{4}$")
issues <- append_issue(
  issues, "H01", "Fatal", ifelse(registry_id_check$ok, "PASS", "FAIL"),
  "The locked effect-level registry has unique, nonmissing E#### identifiers.",
  c(
    registry_id_check$duplicates,
    paste0("row:", registry_id_check$missing_rows),
    paste0("row:", registry_id_check$invalid_rows)
  ),
  "Correct the locked registry."
)

missing_in_registry <- setdiff(effects_formal$`Effect ID`, registry$`Effect ID`)
extra_in_registry <- setdiff(registry$`Effect ID`, effects_formal$`Effect ID`)
issues <- append_issue(
  issues, "H02", "Fatal",
  ifelse(length(missing_in_registry) == 0 && length(extra_in_registry) == 0, "PASS", "FAIL"),
  "The locked registry has exact one-to-one coverage of the 267 validated formal effects.",
  c(missing_in_registry, extra_in_registry),
  "Reconcile effect coverage."
)

issues <- append_issue(
  issues, "H03", "Fatal",
  ifelse(nrow(registry) == config$expected_formal_effects, "PASS", "FAIL"),
  sprintf("Registry contains %d rows; expected %d.",
          nrow(registry), config$expected_formal_effects),
  character(), "Reconcile row count."
)

invalid_codes <- validate_registry_codes(registry, config)
issues <- append_issue(
  issues, "H04", "Fatal", ifelse(nrow(invalid_codes) == 0, "PASS", "FAIL"),
  "All phenotype, comparison, outcome, horizon, unit, measure and role codes are valid.",
  paste(invalid_codes$`Effect ID`, invalid_codes$field, invalid_codes$invalid_value, sep = "|"),
  "Correct invalid operational codes."
)

# Join original effects to the locked registry.
harmonized <- effects_formal |>
  left_join(registry, by = c("Effect ID", "Review ID", "Screening ID")) |>
  parse_effect_quantities()

join_failures <- harmonized |>
  filter(
    is.na(phenotype_code) | is.na(comparison_id) | is.na(outcome_code) |
      is.na(horizon_code) | is.na(unit_code) | is.na(measure_code)
  )
issues <- append_issue(
  issues, "H05", "Fatal", ifelse(nrow(join_failures) == 0, "PASS", "FAIL"),
  "Every formal effect joined to a complete locked harmonization record.",
  join_failures$`Effect ID`, "Repair harmonization join."
)

# Validate immutable cell identifiers.
cell_mismatch <- harmonized |>
  filter(
    evidence_cell_id != evidence_cell_id_rebuilt |
      forest_panel_id != forest_panel_id_rebuilt
  )
issues <- append_issue(
  issues, "H06", "Fatal", ifelse(nrow(cell_mismatch) == 0, "PASS", "FAIL"),
  "Locked evidence-cell and forest-panel identifiers reproduce from component codes.",
  cell_mismatch$`Effect ID`, "Correct cell identifiers."
)

# Validate expected operational distributions.
expected_checks <- tibble(
  check_id = c("H07","H08","H09","H10"),
  description = c(
    "Forest-candidate count",
    "Operational C00 count",
    "Operational O99 count",
    "Comparison-text mismatch count"
  ),
  observed = c(
    sum(harmonized$forest_candidate, na.rm = TRUE),
    sum(harmonized$comparison_id == "C00", na.rm = TRUE),
    sum(harmonized$outcome_code == "O99", na.rm = TRUE),
    sum(harmonized$comparison_text_mismatch, na.rm = TRUE)
  ),
  expected = c(
    config$expected_forest_candidates,
    config$expected_operational_noncomparison_C00,
    config$expected_other_outcome_O99,
    config$expected_comparison_text_mismatch
  )
)

walk(seq_len(nrow(expected_checks)), function(i) {
  row <- expected_checks[i,]
  issues <<- append_issue(
    issues, row$check_id, "Major",
    ifelse(row$observed == row$expected, "PASS", "FAIL"),
    sprintf("%s: observed %d, expected %d.",
            row$description, row$observed, row$expected),
    character(), "Reconcile the locked registry or expected manifest."
  )
})

# Quantitative safety checks.
invalid_ci <- harmonized |>
  filter(
    (ratio_measure | continuous_measure) &
      !is.na(estimate_numeric) &
      !is.na(ci_lower_numeric) & !is.na(ci_upper_numeric) &
      !(ci_lower_numeric <= estimate_numeric & estimate_numeric <= ci_upper_numeric)
  )
invalid_ratio <- harmonized |>
  filter(
    ratio_measure &
      ((!is.na(estimate_numeric) & estimate_numeric <= 0) |
       (!is.na(ci_lower_numeric) & ci_lower_numeric < 0) |
       (!is.na(ci_upper_numeric) & ci_upper_numeric <= 0))
  )
issues <- append_issue(
  issues, "H11", "Fatal", ifelse(nrow(invalid_ci) == 0, "PASS", "FAIL"),
  "Every parseable quantitative point estimate lies inside its confidence interval.",
  invalid_ci$`Effect ID`, "Recheck source effect."
)
issues <- append_issue(
  issues, "H12", "Fatal", ifelse(nrow(invalid_ratio) == 0, "PASS", "FAIL"),
  "All parseable ratio estimates and confidence limits are positive.",
  invalid_ratio$`Effect ID`, "Recheck source effect."
)

forest_invalid <- harmonized |>
  filter(
    forest_candidate &
      (comparison_id == "C00" | outcome_code == "O99" |
       analysis_role != "COMPARATIVE" | !ci_parse_valid)
  )
issues <- append_issue(
  issues, "H13", "Fatal", ifelse(nrow(forest_invalid) == 0, "PASS", "FAIL"),
  "Every forest candidate is comparative, has an authorized comparison, a canonical outcome and a valid CI.",
  forest_invalid$`Effect ID`, "Remove invalid forest candidates."
)

# Evidence-cell and duplicate audit.
evidence_cells <- build_evidence_cells(harmonized)
duplicate_candidates <- build_duplicate_fingerprints(harmonized)

issues <- append_issue(
  issues, "H14", "Moderate", "PASS",
  sprintf("Harmonization produced %d evidence cells from %d formal effects.",
          nrow(evidence_cells), nrow(harmonized)),
  character(), "None."
)
issues <- append_issue(
  issues, "H15", "Moderate",
  ifelse(nrow(duplicate_candidates) == 0, "PASS", "WARNING"),
  sprintf("%d harmonized rows belong to potential within-review duplicate fingerprints.",
          nrow(duplicate_candidates)),
  duplicate_candidates$`Effect ID`,
  "Review during anchor/corroborator selection; do not silently delete."
)

# Analysis derivatives.
forest_candidates <- harmonized |>
  filter(forest_candidate) |>
  arrange(
    phenotype_code, comparison_id, outcome_code,
    horizon_code, unit_code, measure_code, `Review ID`, `Effect ID`
  )

manual_review_queue <- harmonized |>
  filter(manual_review_flag) |>
  arrange(mapping_confidence, phenotype_code, comparison_id, outcome_code, `Effect ID`)

comparative_effects <- harmonized |>
  filter(analysis_role == "COMPARATIVE")

prognostic_effects <- harmonized |>
  filter(analysis_role == "PROGNOSTIC")

descriptive_effects <- harmonized |>
  filter(analysis_role != "COMPARATIVE")

noninferential_operational_rows <- harmonized |>
  filter(comparison_id == "C00" | outcome_code == "O99")

summary_by_code <- bind_rows(
  harmonized |> count(code_family = "Phenotype", code = phenotype_code, name = "n"),
  harmonized |> count(code_family = "Comparison", code = comparison_id, name = "n"),
  harmonized |> count(code_family = "Outcome", code = outcome_code, name = "n"),
  harmonized |> count(code_family = "Horizon", code = horizon_code, name = "n"),
  harmonized |> count(code_family = "Unit", code = unit_code, name = "n"),
  harmonized |> count(code_family = "Measure", code = measure_code, name = "n"),
  harmonized |> count(code_family = "Role", code = analysis_role, name = "n"),
  harmonized |> count(code_family = "Mapping confidence", code = mapping_confidence, name = "n")
) |>
  arrange(code_family, desc(n), code)

fatal_failures <- issues |>
  filter(level == "Fatal", status == "FAIL")

summary <- list(
  harmonization_version = "1.0",
  formal_effects = nrow(harmonized),
  evidence_cells = nrow(evidence_cells),
  forest_candidates = nrow(forest_candidates),
  comparative_effects = nrow(comparative_effects),
  prognostic_effects = nrow(prognostic_effects),
  descriptive_effects = nrow(descriptive_effects),
  manual_review_flags = nrow(manual_review_queue),
  C00_rows = sum(harmonized$comparison_id == "C00"),
  O99_rows = sum(harmonized$outcome_code == "O99"),
  comparison_text_mismatches = sum(harmonized$comparison_text_mismatch),
  fatal_failures = nrow(fatal_failures),
  status = ifelse(nrow(fatal_failures) == 0, "PASS", "FAIL")
)

outputs <- list(
  harmonized_effects = harmonized,
  evidence_cells = evidence_cells,
  forest_candidates = forest_candidates,
  comparative_effects = comparative_effects,
  prognostic_effects = prognostic_effects,
  descriptive_effects = descriptive_effects,
  manual_review_queue = manual_review_queue,
  noninferential_operational_rows = noninferential_operational_rows,
  potential_duplicate_fingerprints = duplicate_candidates,
  harmonization_findings = issues,
  harmonization_code_summary = summary_by_code
)

write_harmonization_outputs(outputs, "derived")
write_json(summary, "derived/harmonization_summary.json", pretty = TRUE, auto_unbox = TRUE)

# Excel limits worksheet names to 31 characters. Use compact display names only
# for the workbook; the RDS/CSV object names above remain unchanged.
excel_outputs <- list(
  harmonized_effects = harmonized,
  evidence_cells = evidence_cells,
  forest_candidates = forest_candidates,
  comparative = comparative_effects,
  prognostic = prognostic_effects,
  descriptive = descriptive_effects,
  manual_review = manual_review_queue,
  noninferential = noninferential_operational_rows,
  duplicate_flags = duplicate_candidates,
  findings = issues,
  code_summary = summary_by_code
)
write_xlsx(excel_outputs, "derived/moyamoya_harmonization_report_R.xlsx")

if (nrow(fatal_failures) > 0) {
  print(fatal_failures)
  cli_abort("Harmonization failed. See derived/harmonization_findings.csv.")
}

message("Harmonization completed successfully.")
message("Formal effects: ", nrow(harmonized))
message("Evidence cells: ", nrow(evidence_cells))
message("Forest candidates: ", nrow(forest_candidates))
message("Manual-review flags retained for transparency: ", nrow(manual_review_queue))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(jsonlite)
  library(writexl)
  library(cli)
})

read_locked_registry <- function(path = "config/effect_harmonization_registry.csv") {
  if (!file.exists(path)) {
    cli_abort("Locked harmonization registry not found: {.file {path}}")
  }
  read_csv(path, show_col_types = FALSE, na = c("", "NA")) |>
    mutate(
      comparison_orientation = as.numeric(comparison_orientation),
      outcome_benefit_multiplier = as.numeric(outcome_benefit_multiplier),
      manual_review_flag = as.logical(manual_review_flag),
      comparison_text_mismatch = as.logical(comparison_text_mismatch)
    )
}

validate_registry_codes <- function(registry, config) {
  code_map <- list(
    phenotype_code = unlist(config$allowed_codes$phenotype),
    comparison_id = unlist(config$allowed_codes$comparison),
    outcome_code = unlist(config$allowed_codes$outcome),
    horizon_code = unlist(config$allowed_codes$horizon),
    unit_code = unlist(config$allowed_codes$unit),
    measure_code = unlist(config$allowed_codes$measure),
    analysis_role = unlist(config$allowed_codes$analysis_role)
  )
  problems <- imap_dfr(code_map, function(allowed, column) {
    registry |>
      filter(is.na(.data[[column]]) | !.data[[column]] %in% allowed) |>
      transmute(
        `Effect ID`,
        field = column,
        invalid_value = as.character(.data[[column]])
      )
  })
  problems
}

parse_effect_quantities <- function(data) {
  ci <- parse_ci_pair(data$`95% CI`)
  data |>
    mutate(
      estimate_numeric = parse_numeric_exact(Estimate),
      ci_lower_numeric = ci$ci_lower,
      ci_upper_numeric = ci$ci_upper,
      ratio_measure = measure_code %in% c("EM_OR", "EM_RR", "EM_HR"),
      continuous_measure = measure_code %in% c("EM_MD", "EM_WMD"),
      ci_parse_valid = !is.na(estimate_numeric) &
        !is.na(ci_lower_numeric) & !is.na(ci_upper_numeric) &
        ci_lower_numeric <= estimate_numeric &
        estimate_numeric <= ci_upper_numeric,
      # Calculate log only after invalid/nonpositive values have been replaced by NA.
      # This avoids the eager-evaluation NaN warning produced by if_else(log(...)).
      log_effect = log(
        if_else(
          ratio_measure & !is.na(estimate_numeric) & estimate_numeric > 0,
          estimate_numeric,
          NA_real_
        )
      ),
      canonical_log_effect = if_else(
        !is.na(log_effect) & comparison_orientation != 0,
        log_effect * comparison_orientation,
        NA_real_
      ),
      benefit_axis_log_effect = if_else(
        !is.na(canonical_log_effect) & outcome_benefit_multiplier != 0,
        canonical_log_effect * outcome_benefit_multiplier,
        NA_real_
      ),
      comparison_authorized = comparison_id != "C00",
      canonical_outcome = outcome_code != "O99",
      forest_candidate = (ratio_measure | continuous_measure) &
        ci_parse_valid &
        comparison_authorized &
        canonical_outcome &
        analysis_role == "COMPARATIVE",
      evidence_cell_id_rebuilt = paste(
        phenotype_code, comparison_id, outcome_code, horizon_code, unit_code,
        sep = "|"
      ),
      forest_panel_id_rebuilt = paste(evidence_cell_id_rebuilt, measure_code, sep = "|")
    )
}

build_evidence_cells <- function(harmonized) {
  harmonized |>
    group_by(
      evidence_cell_id, phenotype_code, comparison_id,
      outcome_code, horizon_code, unit_code
    ) |>
    summarise(
      effect_rows = n(),
      reviews = n_distinct(`Review ID`),
      forest_candidates = sum(forest_candidate, na.rm = TRUE),
      tier1_rows = sum(str_starts(`Master tier`, "Tier 1")),
      tier2_rows = sum(str_starts(`Master tier`, "Tier 2")),
      amstar_low_rows = sum(`Master AMSTAR 2` == "Low", na.rm = TRUE),
      robis_high_rows = sum(`Master ROBIS` == "High", na.rm = TRUE),
      manual_review_rows = sum(manual_review_flag, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(phenotype_code, comparison_id, outcome_code, horizon_code, unit_code)
}

build_duplicate_fingerprints <- function(harmonized) {
  harmonized |>
    mutate(
      duplicate_fingerprint = paste(
        `Review ID`, phenotype_code, comparison_id, outcome_code,
        horizon_code, unit_code, measure_code,
        normalize_minus(Estimate), normalize_minus(`95% CI`),
        sep = "||"
      )
    ) |>
    add_count(duplicate_fingerprint, name = "duplicate_group_size") |>
    filter(duplicate_group_size > 1) |>
    arrange(duplicate_fingerprint, `Effect ID`)
}

write_harmonization_outputs <- function(objects, directory = "derived") {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  walk2(objects, names(objects), function(object, name) {
    saveRDS(object, file.path(directory, paste0(name, ".rds")))
    if (is.data.frame(object)) {
      write_csv(object, file.path(directory, paste0(name, ".csv")), na = "")
    }
  })
}

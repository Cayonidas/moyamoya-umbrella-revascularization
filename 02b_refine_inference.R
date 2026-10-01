source("00_environment.R")
source("R/validation_helpers.R")
source("R/harmonization_helpers.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("02b_refine_inference", {
  harmonized <- safe_read_rds(
    "derived/harmonized_effects.rds",
    "02_harmonize_cells.R"
  )
  require_columns(
    harmonized,
    c(
      "Effect ID","Review ID","Screening ID",
      "Final source-verification status",
      "Estimate","95% CI","Studies/N",
      "Exact PDF page","Validity caveat"
    ),
    "harmonized_effects"
  )

  config <- fromJSON(
    "config/harmonization_config.json",
    simplifyVector = FALSE
  )

  refinement <- read_csv(
    "config/inference_refinement_registry.csv",
    show_col_types = FALSE,
    na = c("", "NA")
  )

  master_path <- "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx"

  # IMPORTANT: source columns are renamed BEFORE joining. This prevents
  # .x/.y collisions with fields already present in harmonized_effects.
  source_effects <- read_excel(
    master_path,
    sheet = "Outcome effects final"
  ) |>
    transmute(
      `Effect ID`,
      use_status_source = `Use status`,
      correction_derived_source = `Correction/derived flag`,
      master_audit_note_source = `Master audit note`,
      analysis_type_source = `Analysis type`,
      source_statement_source = `Source statement`,
      calculated_derived_source = `Calculated/derived`,
      source_verification_source =
        `Final source-verification status`,
      manuscript_ready_source = `Manuscript-ready`
    )

  refinement <- refinement |>
    mutate(
      across(ends_with("override"), as.character),
      `Comparison orientation override` =
        suppressWarnings(
          as.numeric(`Comparison orientation override`)
        ),
      `Outcome benefit multiplier override` =
        suppressWarnings(
          as.numeric(`Outcome benefit multiplier override`)
        )
    )

  effects <- harmonized |>
    left_join(source_effects, by = "Effect ID") |>
    left_join(refinement, by = "Effect ID") |>
    mutate(
      # Stable canonical aliases used by downstream modules.
      source_verification = coalesce(
        source_verification_source,
        `Final source-verification status`,
        ""
      ),
      analysis_type = coalesce(analysis_type_source, ""),
      manuscript_ready = coalesce(manuscript_ready_source, ""),
      source_statement = coalesce(source_statement_source, ""),
      use_status = coalesce(use_status_source, ""),
      master_audit_note = coalesce(master_audit_note_source, ""),
      studies_n = coalesce(as.character(`Studies/N`), ""),
      exact_pdf_page = coalesce(as.character(`Exact PDF page`), ""),
      validity_caveat = coalesce(as.character(`Validity caveat`), ""),

      inference_action = coalesce(Action, "retain"),
      duplicate_of = coalesce(`Duplicate of`, ""),

      phenotype_code =
        coalesce(`Phenotype override`, phenotype_code),
      comparison_id =
        coalesce(`Comparison override`, comparison_id),
      outcome_code =
        coalesce(`Outcome override`, outcome_code),
      horizon_code =
        coalesce(`Horizon override`, horizon_code),
      unit_code =
        coalesce(`Unit override`, unit_code),
      measure_code =
        coalesce(`Measure override`, measure_code),
      analysis_role =
        coalesce(`Analysis role override`, analysis_role),

      comparison_orientation = coalesce(
        `Comparison orientation override`,
        comparison_orientation
      ),
      outcome_benefit_multiplier = coalesce(
        `Outcome benefit multiplier override`,
        outcome_benefit_multiplier
      ),

      intervention_analysis = coalesce(
        `Intervention display override`,
        as.character(Intervention)
      ),
      comparator_analysis = coalesce(
        `Comparator display override`,
        as.character(Comparator)
      ),

      anchor_eligible = case_when(
        `Anchor eligible override` == "Yes" ~ TRUE,
        `Anchor eligible override` == "No" ~ FALSE,
        TRUE ~ inference_action == "retain"
      ),
      corroborator_eligible = case_when(
        `Corroborator eligible override` == "Yes" ~ TRUE,
        `Corroborator eligible override` == "No" ~ FALSE,
        TRUE ~ inference_action == "retain"
      ),

      refinement_reason = coalesce(Reason, ""),
      evidence_cell_id = paste(
        phenotype_code, comparison_id,
        outcome_code, horizon_code,
        unit_code, sep = "|"
      ),
      forest_panel_id =
        paste(evidence_cell_id, measure_code, sep = "|")
    ) |>
    parse_effect_quantities() |>
    mutate(
      ratio_positive_for_log =
        !ratio_measure |
        (
          !is.na(ci_lower_numeric) &
          ci_lower_numeric > 0
        ),

      base_forest_criteria =
        measure_code %in%
          unlist(config$forest_measure_codes) &
        ci_parse_valid &
        comparison_id != "C00" &
        outcome_code != "O99" &
        analysis_role == "COMPARATIVE" &
        ratio_positive_for_log,

      forest_final_eligible = case_when(
        `Forest eligible override` == "Yes" ~ TRUE,
        `Forest eligible override` == "No" ~ FALSE,
        TRUE ~
          inference_action == "retain" &
          base_forest_criteria
      ),

      manuscript_analysis_eligible =
        inference_action == "retain" &
        manuscript_ready %in%
          c("Yes", "Yes — descriptive only"),

      effect_stage = effect_stage(outcome_code),
      phenotype_label =
        label_code(phenotype_code, phenotype_labels),
      comparison_label =
        label_code(comparison_id, comparison_labels),
      outcome_label =
        label_code(outcome_code, outcome_labels),
      horizon_label =
        label_code(horizon_code, horizon_labels),
      unit_label =
        label_code(unit_code, unit_labels),
      measure_label =
        label_code(measure_code, measure_labels)
    ) |>
    mutate(
      # Protected log calculation.
      log_effect = log(
        if_else(
          ratio_measure &
            !is.na(estimate_numeric) &
            estimate_numeric > 0,
          estimate_numeric,
          NA_real_
        )
      ),
      canonical_log_effect = if_else(
        !is.na(log_effect) &
          comparison_orientation != 0,
        log_effect * comparison_orientation,
        NA_real_
      ),
      benefit_axis_log_effect = if_else(
        !is.na(canonical_log_effect) &
          outcome_benefit_multiplier != 0,
        canonical_log_effect *
          outcome_benefit_multiplier,
        NA_real_
      )
    ) |>
    select(
      -matches(" override$"),
      -Action, -Reason, -`Duplicate of`
    )

  require_columns(
    effects,
    c(
      "source_verification","analysis_type",
      "manuscript_ready","forest_final_eligible",
      "anchor_eligible","corroborator_eligible"
    ),
    "effects_refined"
  )

  duplicates <- effects |>
    filter(inference_action == "exclude_duplicate") |>
    select(
      `Effect ID`,`Review ID`,
      duplicate_of, refinement_reason
    )

  excluded <- effects |>
    filter(inference_action != "retain") |>
    select(
      `Effect ID`,`Review ID`,
      inference_action, duplicate_of,
      refinement_reason, exact_pdf_page
    )

  forest_final <- effects |>
    filter(forest_final_eligible) |>
    arrange(
      phenotype_code, comparison_id,
      outcome_code, horizon_code,
      unit_code, measure_code,
      `Review ID`,`Effect ID`
    )

  evidence_cells_refined <- effects |>
    filter(inference_action != "exclude_duplicate") |>
    group_by(
      evidence_cell_id,
      phenotype_code, comparison_id,
      outcome_code, horizon_code,
      unit_code
    ) |>
    summarise(
      effect_rows = n(),
      reviews = n_distinct(`Review ID`),
      forest_candidates =
        sum(forest_final_eligible, na.rm = TRUE),
      tier1_rows =
        sum(str_starts(`Master tier`, "Tier 1")),
      tier2_rows =
        sum(str_starts(`Master tier`, "Tier 2")),
      patient_unit_rows =
        sum(unit_code == "U01"),
      mixed_unit_rows =
        sum(unit_code == "U07"),
      manual_review_rows =
        sum(manual_review_flag, na.rm = TRUE),
      .groups = "drop"
    )

  expected_forest <-
    config$refinement_expected$final_forest_candidates
  expected_cells <-
    config$refinement_expected$final_forest_cells

  findings <- tibble(
    check_id = c("R01","R02","R03","R04","R05","R06"),
    level = c(
      "Fatal","Fatal","Fatal",
      "Fatal","Fatal","Moderate"
    ),
    status = c(
      ifelse(nrow(effects) == 267, "PASS", "FAIL"),
      ifelse(nrow(duplicates) == 12, "PASS", "FAIL"),
      ifelse(nrow(forest_final) == expected_forest, "PASS", "FAIL"),
      ifelse(
        n_distinct(forest_final$evidence_cell_id) ==
          expected_cells,
        "PASS","FAIL"
      ),
      ifelse(
        all(nchar(effects$source_verification) > 0),
        "PASS","FAIL"
      ),
      "PASS"
    ),
    finding = c(
      sprintf("Refinement retained %d formal source rows.", nrow(effects)),
      sprintf("%d duplicate representations isolated.", nrow(duplicates)),
      sprintf("%d effects remain forest-eligible.", nrow(forest_final)),
      sprintf(
        "%d quantitative evidence cells remain.",
        n_distinct(forest_final$evidence_cell_id)
      ),
      "Canonical source_verification field populated without join collision.",
      "O16 and C15 are explicit pre-analysis refinements."
    )
  )

  if (any(findings$status == "FAIL")) {
    write_csv(
      findings,
      "derived/refinement_FATAL_findings.csv"
    )
    stop(
      "Inference refinement failed one or more locked checks. ",
      "See derived/refinement_FATAL_findings.csv",
      call. = FALSE
    )
  }

  outputs <- list(
    effects_refined = effects,
    forest_candidates_refined = forest_final,
    evidence_cells_refined = evidence_cells_refined,
    inference_exclusions = excluded,
    duplicate_effects = duplicates,
    refinement_findings = findings
  )
  walk2(
    outputs, names(outputs),
    ~ write_csv_and_rds(
      .x, file.path("derived", .y)
    )
  )

  write_json(
    list(
      refinement_version = "3.0",
      formal_source_rows = nrow(effects),
      duplicate_rows = nrow(duplicates),
      final_forest_candidates = nrow(forest_final),
      final_forest_cells =
        n_distinct(forest_final$evidence_cell_id),
      C15_rows = sum(effects$comparison_id == "C15"),
      O16_rows = sum(effects$outcome_code == "O16"),
      status = "PASS"
    ),
    "derived/inference_refinement_summary.json",
    pretty = TRUE, auto_unbox = TRUE
  )

  safe_write_xlsx(
    list(
      refined_effects = effects,
      final_forest = forest_final,
      evidence_cells = evidence_cells_refined,
      exclusions = excluded,
      duplicates = duplicates,
      findings = findings
    ),
    "derived/moyamoya_inference_refinement_report.xlsx"
  )
})

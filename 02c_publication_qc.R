source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("02c_publication_qc", {
  effects <- safe_read_rds(
    "derived/effects_refined.rds",
    "02b_refine_inference.R"
  )

  registry <- read_csv(
    "config/publication_source_adjudication_registry.csv",
    show_col_types = FALSE,
    na = c("", "NA")
  )

  if (
    nrow(registry) != 30L ||
    n_distinct(registry$`Effect ID`) != 30L
  ) {
    stop(
      "Publication-QC registry must contain exactly 30 unique effects.",
      call. = FALSE
    )
  }

  registry <- registry |>
    mutate(
      orientation_qc = suppressWarnings(
        as.numeric(`Orientation override`)
      ),
      benefit_multiplier_qc = suppressWarnings(
        as.numeric(`Benefit multiplier override`)
      ),
      final_manual_qc = case_when(
        `Final manual review flag` == "TRUE" ~ TRUE,
        `Final manual review flag` == "FALSE" ~ FALSE,
        TRUE ~ NA
      ),
      final_mismatch_qc = case_when(
        `Final comparison mismatch flag` == "TRUE" ~ TRUE,
        `Final comparison mismatch flag` == "FALSE" ~ FALSE,
        TRUE ~ NA
      )
    ) |>
    transmute(
      `Effect ID`,
      comparison_qc = `Comparison override`,
      orientation_qc,
      benefit_multiplier_qc,
      intervention_qc = `Source intervention override`,
      comparator_qc = `Source comparator override`,
      mapping_qc = `Mapping confidence override`,
      final_manual_qc,
      final_mismatch_qc,
      adjudication_status_qc =
        `Final source adjudication status`,
      direction_qc =
        `Direction interpretation override`,
      population_note_qc =
        `Population note override`,
      adjudication_note_qc =
        `Source-adjudication note`
    )

  effects_final <- effects |>
    left_join(registry, by = "Effect ID") |>
    mutate(
      comparison_id = coalesce(
        comparison_qc,
        comparison_id
      ),
      comparison_orientation = coalesce(
        orientation_qc,
        comparison_orientation
      ),
      outcome_benefit_multiplier = coalesce(
        benefit_multiplier_qc,
        outcome_benefit_multiplier
      ),
      intervention_analysis = coalesce(
        intervention_qc,
        intervention_analysis
      ),
      comparator_analysis = coalesce(
        comparator_qc,
        comparator_analysis
      ),
      mapping_confidence = coalesce(
        mapping_qc,
        mapping_confidence
      ),
      final_manual_review_flag = coalesce(
        final_manual_qc,
        manual_review_flag
      ),
      final_comparison_text_mismatch = coalesce(
        final_mismatch_qc,
        comparison_text_mismatch
      ),
      final_source_adjudication_status = coalesce(
        adjudication_status_qc,
        if_else(
          manual_review_flag,
          "NOT_REQUIRED_AFTER_INITIAL_HARMONIZATION",
          "NOT_REQUIRED"
        )
      ),
      publication_qc_note = coalesce(
        adjudication_note_qc,
        ""
      ),
      publication_population_note = coalesce(
        population_note_qc,
        ""
      ),
      `Direction/interpretation` = coalesce(
        direction_qc,
        `Direction/interpretation`
      ),
      evidence_cell_id = paste(
        phenotype_code,
        comparison_id,
        outcome_code,
        horizon_code,
        unit_code,
        sep = "|"
      ),
      forest_panel_id = paste(
        evidence_cell_id,
        measure_code,
        sep = "|"
      ),
      comparison_label = label_code(
        comparison_id,
        comparison_labels
      )
    ) |>
    select(
      -comparison_qc,
      -orientation_qc,
      -benefit_multiplier_qc,
      -intervention_qc,
      -comparator_qc,
      -mapping_qc,
      -final_manual_qc,
      -final_mismatch_qc,
      -adjudication_status_qc,
      -direction_qc,
      -population_note_qc,
      -adjudication_note_qc
    ) |>
    canonicalize_effect_display() |>
    mutate(
      `Direction/interpretation` = if_else(
        `Effect ID` == "E0296",
        "Higher perioperative ischemic stroke risk; source meta-analysis reports OR 2.62 (95% CI 1.36–5.06).",
        `Direction/interpretation`
      )
    )

  forest_final <- effects_final |>
    filter(forest_final_eligible) |>
    arrange(
      phenotype_code,
      comparison_id,
      outcome_code,
      horizon_code,
      unit_code,
      measure_code,
      `Review ID`,
      `Effect ID`
    )

  qc_registry_audit <- effects_final |>
    semi_join(
      registry |> select(`Effect ID`),
      by = "Effect ID"
    ) |>
    transmute(
      `Effect ID`,
      `Review ID`,
      comparison_id,
      outcome_code,
      comparison_orientation,
      outcome_benefit_multiplier,
      intervention_analysis,
      comparator_analysis,
      mapping_confidence,
      manual_review_flag,
      final_manual_review_flag,
      comparison_text_mismatch,
      final_comparison_text_mismatch,
      final_source_adjudication_status,
      publication_conclusion_class,
      canonical_effect_text,
      source_effect_text,
      display_orientation_changed,
      exact_pdf_page,
      publication_qc_note
    )

  findings <- tibble(
    Check = c(
      "Formal effect rows preserved",
      "Forest-eligible rows preserved",
      "Quantitative evidence cells preserved",
      "Publication-QC registry complete",
      "R017 anesthesia mismatch resolved",
      "No nonpositive ratio CI in final forest",
      "Continuous mRS classified clinically",
      "No nonfinite canonical display estimate"
    ),
    Status = c(
      ifelse(nrow(effects_final) == 267L, "PASS","FAIL"),
      ifelse(nrow(forest_final) == 72L, "PASS","FAIL"),
      ifelse(
        n_distinct(forest_final$evidence_cell_id) == 60L,
        "PASS","FAIL"
      ),
      ifelse(nrow(qc_registry_audit) == 30L, "PASS","FAIL"),
      ifelse(
        all(
          !effects_final$final_comparison_text_mismatch[
            effects_final$`Effect ID` %in%
              c("E0178","E0277")
          ]
        ),
        "PASS","FAIL"
      ),
      ifelse(
        all(
          !forest_final$ratio_measure |
            (
              !is.na(forest_final$display_ci_lower) &
              forest_final$display_ci_lower > 0
            )
        ),
        "PASS","FAIL"
      ),
      ifelse(
        effects_final$publication_conclusion_class[
          effects_final$`Effect ID` == "E0265"
        ] ==
          "Borderline benefit direction (CI boundary at null)",
        "PASS","FAIL"
      ),
      ifelse(
        all(
          is.na(forest_final$display_estimate) |
            is.finite(forest_final$display_estimate)
        ),
        "PASS","FAIL"
      )
    )
  )

  if (any(findings$Status == "FAIL")) {
    write_csv(
      findings,
      "derived/PUBLICATION_QC_FAILURE.csv"
    )
    stop(
      "Publication-final source QC failed. See derived/PUBLICATION_QC_FAILURE.csv",
      call. = FALSE
    )
  }

  outputs <- list(
    effects_publication_final = effects_final,
    forest_candidates_publication_final = forest_final,
    publication_qc_registry_audit = qc_registry_audit,
    publication_qc_findings = findings
  )

  walk2(
    outputs,
    names(outputs),
    ~ write_csv_and_rds(
      .x,
      file.path("derived", .y)
    )
  )

  safe_write_xlsx(
    list(
      registry_audit = qc_registry_audit,
      findings = findings,
      final_forest = forest_final
    ),
    "results/tables/02c_publication_qc_audit.xlsx"
  )

  write_json(
    list(
      module = "02c_publication_qc",
      formal_effects = nrow(effects_final),
      registry_effects = nrow(qc_registry_audit),
      final_forest_effects = nrow(forest_final),
      final_quantitative_cells =
        n_distinct(forest_final$evidence_cell_id),
      status = "PASS"
    ),
    "derived/publication_qc_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message("Publication-final source adjudication completed.")
})

source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")
source("R/anchor_helpers.R")

run_logged_module("04_anchor_selection", {
  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  )
  review_master <- safe_read_rds(
    "derived/review_master_validated.rds",
    "01_import_validate.R"
  )

  candidates_pre <- prepare_anchor_candidates(
    effects,
    review_master
  )

  write_csv_and_rds(
    candidates_pre,
    "derived/anchor_candidates_preselection"
  )

  ranked <- select_anchors(
    effects,
    review_master
  ) |>
    add_conclusion_class() |>
    mutate(
      phenotype_label =
        label_code(
          phenotype_code,
          phenotype_labels
        ),
      comparison_label =
        label_code(
          comparison_id,
          comparison_labels
        ),
      outcome_label =
        label_code(
          outcome_code,
          outcome_labels
        ),
      horizon_label =
        label_code(
          horizon_code,
          horizon_labels
        ),
      unit_label =
        label_code(
          unit_code,
          unit_labels
        ),
      measure_label =
        label_code(
          measure_code,
          measure_labels
        ),
      anchor_selection_reason =
        paste(
          "analysis rank",
          analysis_rank,
          "| search year",
          search_year,
          "| relevant studies",
          relevant_studies,
          "| source verification rank",
          source_rank,
          "| AMSTAR",
          `Master AMSTAR 2`,
          "| ROBIS",
          `Master ROBIS`,
          "| unit",
          unit_code
        )
    )

  anchors <- ranked |>
    filter(
      anchor_role ==
        "Anchor"
    )

  corroborators <- ranked |>
    filter(
      anchor_role ==
        "Corroborator"
    )

  additional <- ranked |>
    filter(
      anchor_role ==
        "Additional eligible review"
    )

  conclusion_counts <- anchors |>
    count(
      publication_conclusion_class,
      name = "Anchors"
    )

  expected_counts <- tibble(
    publication_conclusion_class = c(
      "Precise benefit",
      "Imprecise benefit direction",
      "Imprecise harm direction",
      "Precise harm",
      "Borderline benefit direction (CI boundary at null)"
    ),
    Expected =
      c(39L,15L,3L,2L,1L)
  )

  conclusion_check <- expected_counts |>
    left_join(
      conclusion_counts,
      by =
        "publication_conclusion_class"
    ) |>
    mutate(
      Anchors =
        coalesce(
          Anchors,
          0L
        ),
      Match =
        Anchors ==
        Expected
    )

  findings <- tibble(
    Check = c(
      "Exactly 60 anchors",
      "Exactly 60 unique evidence cells",
      "Exactly 7 corroborators",
      "No unresolved manual-review anchor flag",
      "No anchor comparator-text mismatch",
      "No anchor comparison orientation equals zero",
      "No anchor is directionally unclassified",
      "Expected publication-final conclusion distribution",
      "E0265 classified as borderline functional benefit",
      "All canonical ratio CI limits are positive"
    ),
    Status = c(
      ifelse(
        nrow(anchors) == 60L,
        "PASS","FAIL"
      ),
      ifelse(
        n_distinct(
          anchors$evidence_cell_id
        ) == 60L,
        "PASS","FAIL"
      ),
      ifelse(
        nrow(corroborators) == 7L,
        "PASS","FAIL"
      ),
      ifelse(
        all(
          !anchors$final_manual_review_flag
        ),
        "PASS","FAIL"
      ),
      ifelse(
        all(
          !anchors$final_comparison_text_mismatch
        ),
        "PASS","FAIL"
      ),
      ifelse(
        all(
          anchors$comparison_orientation != 0
        ),
        "PASS","FAIL"
      ),
      ifelse(
        all(
          anchors$publication_conclusion_class !=
            "Direction not prespecified"
        ),
        "PASS","FAIL"
      ),
      ifelse(
        all(
          conclusion_check$Match
        ),
        "PASS","FAIL"
      ),
      ifelse(
        anchors$publication_conclusion_class[
          anchors$`Effect ID` ==
            "E0265"
        ] ==
          "Borderline benefit direction (CI boundary at null)",
        "PASS","FAIL"
      ),
      ifelse(
        all(
          !anchors$ratio_measure |
            (
              !is.na(
                anchors$display_ci_lower
              ) &
              anchors$display_ci_lower >
                0 &
              !is.na(
                anchors$display_ci_upper
              ) &
              anchors$display_ci_upper >
                0
            )
        ),
        "PASS","FAIL"
      )
    )
  )

  if (
    any(
      findings$Status ==
        "FAIL"
    ) ||
    any(
      !conclusion_check$Match
    )
  ) {
    write_csv(
      findings,
      "derived/ANCHOR_PUBLICATION_QC_FAILURE.csv"
    )
    write_csv(
      conclusion_check,
      "derived/ANCHOR_CONCLUSION_DISTRIBUTION_FAILURE.csv"
    )
    stop(
      "Publication-final anchor QC failed. See derived/ANCHOR_PUBLICATION_QC_FAILURE.csv",
      call. = FALSE
    )
  }

  anchor_registry <- ranked |>
    transmute(
      evidence_cell_id,
      phenotype_code,
      phenotype_label,
      comparison_id,
      comparison_label,
      outcome_code,
      outcome_label,
      horizon_code,
      horizon_label,
      unit_code,
      unit_label,
      `Effect ID`,
      `Review ID`,
      `Review title`,
      anchor_role,
      final_rank,
      measure_code,
      measure_label,
      source_estimate_numeric,
      source_ci_lower_numeric,
      source_ci_upper_numeric,
      display_estimate,
      display_ci_lower,
      display_ci_upper,
      source_effect_text,
      canonical_effect_text,
      display_intervention,
      display_comparator,
      publication_conclusion_class,
      publication_precision_class,
      `Master AMSTAR 2`,
      `Master ROBIS`,
      search_year,
      relevant_studies,
      analysis_type,
      source_verification,
      final_source_adjudication_status,
      final_manual_review_flag,
      final_comparison_text_mismatch,
      exact_pdf_page,
      publication_qc_note,
      anchor_selection_reason,
      manual_anchor_override
    ) |>
    arrange(
      phenotype_code,
      comparison_id,
      outcome_code,
      horizon_code,
      unit_code,
      final_rank
    )

  cell_audit <- ranked |>
    group_by(
      evidence_cell_id
    ) |>
    summarise(
      candidate_reviews = n(),
      anchor_effect =
        first(
          `Effect ID`[
            anchor_role ==
              "Anchor"
          ]
        ),
      anchor_review =
        first(
          `Review ID`[
            anchor_role ==
              "Anchor"
          ]
        ),
      corroborator_reviews =
        paste(
          `Review ID`[
            anchor_role ==
              "Corroborator"
          ],
          collapse = "; "
        ),
      conclusion =
        first(
          publication_conclusion_class[
            anchor_role ==
              "Anchor"
          ]
        ),
      .groups = "drop"
    )

  outputs <- list(
    anchor_registry =
      anchor_registry,
    anchor_effects =
      anchors,
    corroborator_effects =
      corroborators,
    additional_eligible_reviews =
      additional,
    anchor_cell_audit =
      cell_audit,
    anchor_findings =
      findings,
    anchor_conclusion_distribution =
      conclusion_counts
  )

  walk2(
    outputs,
    names(outputs),
    ~ write_csv_and_rds(
      .x,
      file.path(
        "derived",
        .y
      )
    )
  )

  safe_write_xlsx(
    list(
      registry =
        anchor_registry,
      cell_audit =
        cell_audit,
      anchors =
        anchors,
      corroborators =
        corroborators,
      conclusion_distribution =
        conclusion_counts,
      findings =
        findings
    ),
    "results/tables/04_anchor_selection_audit.xlsx"
  )

  write_json(
    list(
      module =
        "04_anchor_selection",
      quantitative_cells =
        nrow(anchors),
      anchors =
        nrow(anchors),
      corroborators =
        nrow(corroborators),
      cells_with_multiple_reviews =
        sum(
          cell_audit$candidate_reviews >
            1
        ),
      unresolved_anchor_flags =
        sum(
          anchors$final_manual_review_flag
        ),
      comparator_mismatches =
        sum(
          anchors$final_comparison_text_mismatch
        ),
      status = "PASS"
    ),
    "derived/anchor_selection_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Publication-final anchor selection completed."
  )
})

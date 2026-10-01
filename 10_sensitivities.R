source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")
source("R/anchor_helpers.R")

run_logged_module("10_sensitivities", {
  effects <- safe_read_rds(
    "derived/effects_publication_final.rds", "02c_publication_qc.R"
  )
  reviews <- safe_read_rds(
    "derived/review_master_validated.rds", "01_import_validate.R"
  )
  baseline <- safe_read_rds(
    "derived/anchor_effects.rds", "04_anchor_selection.R"
  ) |>
    add_conclusion_class() |>
    select(
      evidence_cell_id,
      baseline_effect = `Effect ID`,
      baseline_review = `Review ID`,
      baseline_conclusion = conclusion_class,
      baseline_precision = precision_class
    )

  latest_filter <- if (file.exists(
    "derived/latest_review_family_filter.rds"
  )) {
    readRDS("derived/latest_review_family_filter.rds")
  } else {
    tibble(
      `Review ID` = reviews$`Review ID`,
      latest_in_family = TRUE,
      family_size = 1L
    )
  }

  review_flags <- reviews |>
    transmute(
      `Review ID`,
      hybrid = str_to_lower(coalesce(`Hybrid report`, "")) != "no",
      duplicate_process =
        yes_like(`Selection in duplicate`) &
        yes_like(`Extraction in duplicate`),
      asia_dominant = str_detect(
        str_to_lower(paste(
          coalesce(Region, ""),
          coalesce(`Countries/centers`, "")
        )),
        "japan|china|korea|taiwan|asia|hong kong|singapore"
      ),
      geographically_mixed = str_detect(
        str_to_lower(paste(
          coalesce(Region, ""),
          coalesce(`Countries/centers`, "")
        )),
        "multinational|united states|usa|europe|canada|australia|mixed"
      )
    ) |>
    left_join(latest_filter, by = "Review ID")

  effects_flagged <- effects |>
    left_join(review_flags, by = "Review ID")

  select_sensitivity <- function(data, sensitivity_id, label) {
    eligible <- data |>
      filter(
        coalesce(forest_final_eligible, FALSE),
        coalesce(corroborator_eligible, FALSE)
      )

    if (!nrow(eligible)) {
      return(tibble(
        sensitivity_id = character(),
        sensitivity_label = character(),
        evidence_cell_id = character(),
        sensitivity_effect = character(),
        sensitivity_review = character(),
        sensitivity_conclusion = character(),
        sensitivity_precision = character()
      ))
    }

    selected <- select_anchors(eligible, reviews)

    if (!nrow(selected)) {
      return(tibble(
        sensitivity_id = character(),
        sensitivity_label = character(),
        evidence_cell_id = character(),
        sensitivity_effect = character(),
        sensitivity_review = character(),
        sensitivity_conclusion = character(),
        sensitivity_precision = character()
      ))
    }

    selected |>
      filter(anchor_role == "Anchor") |>
      add_conclusion_class() |>
      transmute(
        sensitivity_id = sensitivity_id,
        sensitivity_label = label,
        evidence_cell_id,
        sensitivity_effect = `Effect ID`,
        sensitivity_review = `Review ID`,
        sensitivity_conclusion = conclusion_class,
        sensitivity_precision = precision_class
      )
  }

  sensitivity_sets <- list(
    S01_Tier1 = effects_flagged |>
      filter(str_starts(`Master tier`, "Tier 1")),
    S03_latest_family = effects_flagged |>
      filter(latest_in_family),
    S04_no_hybrid = effects_flagged |>
      filter(!hybrid),
    S05_AMSTAR_Low = effects_flagged |>
      filter(`Master AMSTAR 2` == "Low"),
    S06_ROBIS_Unclear = effects_flagged |>
      filter(`Master ROBIS` == "Unclear"),
    S07_idiopathic = effects_flagged |>
      filter(
        phenotype_code %in% c(
          "ADULT_ISCH","ADULT_HEM","ADULT_MIX",
          "PED_ISCH","ASYM_STABLE"
        )
      ),
    S08_patient_unit = effects_flagged |>
      filter(unit_code == "U01"),
    S09_comparative = effects_flagged |>
      filter(analysis_role == "COMPARATIVE"),
    S10_defined_horizon = effects_flagged |>
      filter(horizon_code %in% c("T0","T1","T2","T3")),
    S12_Asia_dominant = effects_flagged |>
      filter(asia_dominant),
    S12_geographically_mixed = effects_flagged |>
      filter(geographically_mixed),
    S14_duplicate_process = effects_flagged |>
      filter(duplicate_process)
  )

  sensitivity_labels <- c(
    S01_Tier1 = "Tier 1 only",
    S03_latest_family = "Latest review per editorial family",
    S04_no_hybrid = "Exclude hybrid reports",
    S05_AMSTAR_Low = "AMSTAR 2 Low only",
    S06_ROBIS_Unclear = "ROBIS Unclear only",
    S07_idiopathic = "Idiopathic MMD phenotypes only",
    S08_patient_unit = "Patient-level analytical units only",
    S09_comparative = "Comparative effects only",
    S10_defined_horizon = "Exclude mixed/not-reported horizons",
    S12_Asia_dominant = "Asia-dominant evidence",
    S12_geographically_mixed = "Geographically mixed evidence",
    S14_duplicate_process = "Duplicate selection and extraction"
  )

  sensitivity_anchors <- imap_dfr(
    sensitivity_sets,
    ~ select_sensitivity(.x, .y, sensitivity_labels[[.y]])
  )

  compare_sets <- sensitivity_anchors |>
    right_join(
      crossing(
        sensitivity_id = names(sensitivity_sets),
        evidence_cell_id = baseline$evidence_cell_id
      ),
      by = c("sensitivity_id","evidence_cell_id")
    ) |>
    left_join(
      tibble(
        sensitivity_id = names(sensitivity_sets),
        sensitivity_label = unname(sensitivity_labels)
      ),
      by = "sensitivity_id",
      suffix = c("", ".lookup")
    ) |>
    mutate(
      sensitivity_label = coalesce(
        sensitivity_label, sensitivity_label.lookup
      )
    ) |>
    select(-sensitivity_label.lookup) |>
    left_join(baseline, by = "evidence_cell_id") |>
    mutate(
      change_class = case_when(
        is.na(sensitivity_review) ~ "Not estimable",
        sensitivity_conclusion == baseline_conclusion &
          sensitivity_precision == baseline_precision ~ "Unchanged",
        str_detect(sensitivity_conclusion, "benefit") &
          str_detect(baseline_conclusion, "harm") ~ "Direction reversal",
        str_detect(sensitivity_conclusion, "harm") &
          str_detect(baseline_conclusion, "benefit") ~ "Direction reversal",
        sensitivity_precision != baseline_precision ~ "Precision changed",
        TRUE ~ "Conclusion category changed"
      )
    )

  sensitivity_summary <- compare_sets |>
    count(
      sensitivity_id, sensitivity_label, change_class,
      name = "Cells"
    ) |>
    group_by(sensitivity_id, sensitivity_label) |>
    mutate(Percent = Cells / sum(Cells) * 100) |>
    ungroup()

  # S11: leave each high-priority cohort family out.
  cohort_map <- if (file.exists(
    "derived/cohort_family_review_map.rds"
  )) {
    readRDS("derived/cohort_family_review_map.rds")
  } else tibble()

  family_results <- tibble(
    sensitivity_id = character(),
    sensitivity_label = character(),
    evidence_cell_id = character(),
    sensitivity_effect = character(),
    sensitivity_review = character(),
    sensitivity_conclusion = character(),
    sensitivity_precision = character(),
    baseline_effect = character(),
    baseline_review = character(),
    baseline_conclusion = character(),
    baseline_precision = character(),
    excluded_reviews = character(),
    change_class = character()
  )
  if (nrow(cohort_map)) {
    high_families <- cohort_map |>
      filter(Priority == "High") |>
      distinct(`Cohort family ID`)
    family_results <- map_dfr(
      high_families$`Cohort family ID`,
      function(family_id) {
        excluded_reviews <- cohort_map |>
          filter(`Cohort family ID` == family_id) |>
          pull(`Review ID`) |>
          unique()
        sensitivity <- select_sensitivity(
          effects_flagged |>
            filter(!`Review ID` %in% excluded_reviews),
          paste0("S11_", family_id),
          paste0("Exclude cohort family ", family_id)
        )
        sensitivity |>
          left_join(baseline, by = "evidence_cell_id") |>
          mutate(
            excluded_reviews = paste(excluded_reviews, collapse = "; "),
            change_class = case_when(
              sensitivity_conclusion == baseline_conclusion &
                sensitivity_precision == baseline_precision ~ "Unchanged",
              str_detect(sensitivity_conclusion, "benefit") &
                str_detect(baseline_conclusion, "harm") ~ "Direction reversal",
              str_detect(sensitivity_conclusion, "harm") &
                str_detect(baseline_conclusion, "benefit") ~ "Direction reversal",
              sensitivity_precision != baseline_precision ~ "Precision changed",
              TRUE ~ "Conclusion category changed"
            )
          )
      }
    )
  }

  # S13: leave-one-review-out within cells containing at least three eligible reviews.
  candidate_ranked <- select_anchors(effects_flagged, reviews)
  multi_cells <- candidate_ranked |>
    count(evidence_cell_id, name = "candidate_reviews") |>
    filter(candidate_reviews >= 3)
  loo_results <- tibble(
    sensitivity_id = character(),
    sensitivity_label = character(),
    evidence_cell_id = character(),
    sensitivity_effect = character(),
    sensitivity_review = character(),
    sensitivity_conclusion = character(),
    sensitivity_precision = character(),
    excluded_review = character(),
    baseline_effect = character(),
    baseline_review = character(),
    baseline_conclusion = character(),
    baseline_precision = character(),
    change_class = character()
  )
  if (nrow(multi_cells)) {
    loo_results <- map_dfr(
      multi_cells$evidence_cell_id,
      function(cell) {
        cell_candidates <- effects_flagged |>
          filter(evidence_cell_id == cell, forest_final_eligible)
        review_ids <- unique(cell_candidates$`Review ID`)
        map_dfr(review_ids, function(excluded_review) {
          selected <- select_sensitivity(
            cell_candidates |>
              filter(`Review ID` != excluded_review),
            "S13_leave_one_review_out",
            "Leave one review out"
          )
          selected |>
            mutate(excluded_review = excluded_review)
        })
      }
    ) |>
      left_join(baseline, by = "evidence_cell_id") |>
      mutate(
        change_class = case_when(
          sensitivity_conclusion == baseline_conclusion &
            sensitivity_precision == baseline_precision ~ "Unchanged",
          str_detect(sensitivity_conclusion, "benefit") &
            str_detect(baseline_conclusion, "harm") ~ "Direction reversal",
          str_detect(sensitivity_conclusion, "harm") &
            str_detect(baseline_conclusion, "benefit") ~ "Direction reversal",
          sensitivity_precision != baseline_precision ~ "Precision changed",
          TRUE ~ "Conclusion category changed"
        )
      )
  }

  heatmap_data <- sensitivity_summary |>
    filter(change_class %in% c(
      "Unchanged","Precision changed",
      "Conclusion category changed","Direction reversal","Not estimable"
    ))

  p_sensitivity <- ggplot(
    heatmap_data,
    aes(change_class, sensitivity_label, fill = Percent)
  ) +
    geom_tile(colour = "white", linewidth = 0.35) +
    geom_text(aes(label = sprintf("%.0f%%", Percent)), size = 3) +
    scale_fill_gradient(low = "#EAF3F8", high = "#17365D") +
    labs(
      title = "Robustness of anchor-review conclusions",
      x = NULL, y = NULL, fill = "Cells"
    ) +
    theme_moyamoya(9) +
    theme(
      axis.text.x = element_text(angle = 40, hjust = 1),
      panel.grid = element_blank()
    )
  save_plot_multiformat(
    p_sensitivity,
    "results/figures/main/Figure_sensitivity_matrix",
    11.5, 8.5
  )

  outputs <- list(
    baseline_anchor_conclusions = baseline,
    sensitivity_anchor_sets = sensitivity_anchors,
    sensitivity_cell_comparison = compare_sets,
    sensitivity_summary = sensitivity_summary,
    cohort_family_leave_out = family_results,
    leave_one_review_out = loo_results
  )
  walk2(outputs, names(outputs), ~ write_csv_and_rds(.x, file.path("derived", .y)))
  safe_write_xlsx(
    outputs,
    "results/tables/10_sensitivity_tables.xlsx"
  )
  write_json(
    list(
      module = "10_sensitivities",
      standard_sensitivities = length(sensitivity_sets),
      baseline_cells = nrow(baseline),
      family_sensitivities =
        n_distinct(family_results$sensitivity_id),
      leave_one_review_out_runs = nrow(loo_results),
      direction_reversals =
        sum(compare_sets$change_class == "Direction reversal", na.rm = TRUE),
      status = "PASS"
    ),
    "derived/sensitivity_summary.json",
    pretty = TRUE, auto_unbox = TRUE
  )
  message("Sensitivity module completed.")
})

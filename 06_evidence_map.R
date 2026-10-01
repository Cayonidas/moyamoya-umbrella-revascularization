source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("06_evidence_map", {
  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  ) |>
    filter(
      inference_action !=
        "exclude_duplicate"
    )

  context_data <- effects |>
    filter(
      comparison_id %in%
        c(
          "C01","C02","C03","C04","C05",
          "C06","C08","C09","C10","C11",
          "C15"
        )
    ) |>
    mutate(
      decision_context =
        case_when(
          comparison_id == "C01" ~
            "Adult symptomatic: surgery vs conservative",
          comparison_id == "C06" ~
            "Hemorrhagic MMD: revascularization",
          comparison_id == "C02" ~
            "Adult: direct vs indirect",
          comparison_id == "C03" ~
            "Adult: combined vs indirect",
          comparison_id == "C04" ~
            "Adult: direct vs combined",
          comparison_id == "C15" ~
            "Adult: direct/combined vs indirect",
          comparison_id == "C05" ~
            "Pediatric: direct/combined vs indirect",
          comparison_id == "C08" ~
            "Asymptomatic/stable: surgery vs surveillance",
          comparison_id == "C09" ~
            "Antiplatelet vs none/alternative",
          comparison_id == "C10" ~
            "Anesthesia: inhalational vs intravenous",
          comparison_id == "C11" ~
            "Remote ischemic conditioning vs control",
          TRUE ~
            comparison_label
        ),
      outcome_domain =
        case_when(
          outcome_code == "O01" ~
            "Early ischemia",
          outcome_code == "O04" ~
            "Early hemorrhage",
          outcome_code %in%
            c("O02","O16") ~
            "Late/recurrent stroke",
          outcome_code == "O05" ~
            "Late hemorrhage",
          outcome_code == "O03" ~
            "TIA",
          outcome_code == "O06" ~
            "Function",
          outcome_code == "O07" ~
            "Mortality",
          outcome_code %in%
            c("O10","O11") ~
            "Angiography/patency",
          outcome_code == "O12" ~
            "Hemodynamics",
          TRUE ~
            NA_character_
        )
    ) |>
    filter(
      !is.na(
        outcome_domain
      )
    )

  map_data <- context_data |>
    group_by(
      decision_context,
      outcome_domain
    ) |>
    summarise(
      effect_rows = n(),
      reviews =
        n_distinct(
          `Review ID`
        ),
      forest_candidates =
        sum(
          forest_final_eligible,
          na.rm = TRUE
        ),
      low_amstar_rows =
        sum(
          `Master AMSTAR 2` ==
            "Low"
        ),
      high_robis_rows =
        sum(
          `Master ROBIS` ==
            "High"
        ),
      .groups =
        "drop"
    ) |>
    mutate(
      relative_confidence =
        case_when(
          low_amstar_rows > 0 &
            high_robis_rows == 0 ~
              "Relatively stronger",
          low_amstar_rows > 0 ~
              "Mixed/limited",
          TRUE ~
              "Very limited"
        ),
      forest_status =
        if_else(
          forest_candidates > 0,
          "Comparative estimate with CI",
          "No forest-ready estimate"
        )
    )

  context_levels <- c(
    "Adult symptomatic: surgery vs conservative",
    "Hemorrhagic MMD: revascularization",
    "Adult: direct vs indirect",
    "Adult: combined vs indirect",
    "Adult: direct vs combined",
    "Adult: direct/combined vs indirect",
    "Pediatric: direct/combined vs indirect",
    "Asymptomatic/stable: surgery vs surveillance",
    "Antiplatelet vs none/alternative",
    "Anesthesia: inhalational vs intravenous",
    "Remote ischemic conditioning vs control"
  )

  outcome_levels <- c(
    "Early ischemia",
    "Early hemorrhage",
    "Late/recurrent stroke",
    "Late hemorrhage",
    "TIA",
    "Function",
    "Mortality",
    "Angiography/patency",
    "Hemodynamics"
  )

  map_data <- map_data |>
    mutate(
      decision_context =
        factor(
          decision_context,
          levels =
            rev(
              context_levels
            )
        ),
      outcome_domain =
        factor(
          outcome_domain,
          levels =
            outcome_levels
        )
    )

  p_map <- ggplot(
    map_data,
    aes(
      x =
        outcome_domain,
      y =
        decision_context,
      size =
        reviews,
      fill =
        relative_confidence,
      alpha =
        forest_status
    )
  ) +
    geom_point(
      shape = 21,
      colour =
        "#404040",
      stroke = 0.35
    ) +
    scale_size_continuous(
      range =
        c(3.0,10.5),
      breaks =
        c(1,2,3,5,10)
    ) +
    scale_fill_manual(
      values = c(
        "Relatively stronger" =
          "#70AD47",
        "Mixed/limited" =
          "#FFD966",
        "Very limited" =
          "#F4B183"
      )
    ) +
    scale_alpha_manual(
      values = c(
        "Comparative estimate with CI" =
          1,
        "No forest-ready estimate" =
          0.38
      )
    ) +
    labs(
      title =
        "Figure 2. Decision-context evidence map for moyamoya revascularization",
      subtitle =
        "Bubble size indicates contributing reviews; opacity identifies directly displayable comparative evidence",
      x = NULL,
      y = NULL,
      size =
        "Reviews",
      fill =
        "Relative review confidence",
      alpha = NULL,
      caption =
        "Relative confidence is comparative within this review corpus, not a GRADE certainty rating."
    ) +
    theme_moyamoya(9.5) +
    theme(
      axis.text.x =
        element_text(
          angle = 45,
          hjust = 1
        ),
      panel.grid.minor =
        element_blank(),
      panel.grid.major =
        element_line(
          colour =
            "#EFEFEF",
          linewidth =
            0.35
        ),
      legend.box =
        "vertical"
    )

  save_plot_multiformat(
    p_map,
    "results/figures/main/Figure_2_phenotype_evidence_map",
    13.8,
    8.2
  )

  full_map <- effects |>
    group_by(
      phenotype_code,
      comparison_id,
      outcome_code
    ) |>
    summarise(
      effects = n(),
      reviews =
        n_distinct(
          `Review ID`
        ),
      forest =
        sum(
          forest_final_eligible
        ),
      .groups =
        "drop"
    ) |>
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
        )
    )

  gap_table <- context_data |>
    group_by(
      decision_context,
      outcome_domain
    ) |>
    summarise(
      reviews =
        n_distinct(
          `Review ID`
        ),
      forest_candidates =
        sum(
          forest_final_eligible
        ),
      mixed_unit_rows =
        sum(
          unit_code ==
            "U07"
        ),
      unspecified_horizon_rows =
        sum(
          horizon_code %in%
            c("T4","T9")
        ),
      .groups =
        "drop"
    ) |>
    mutate(
      gap_class =
        case_when(
          forest_candidates == 0 ~
            "No forest-ready comparative estimate",
          reviews == 1 ~
            "Single-review quantitative evidence",
          mixed_unit_rows > 0 |
            unspecified_horizon_rows > 0 ~
            "Available but structurally heterogeneous",
          TRUE ~
            "Comparative evidence available"
        )
    )

  outputs <- list(
    evidence_map_data =
      map_data,
    full_evidence_map =
      full_map,
    evidence_gap_table =
      gap_table
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
    outputs,
    "results/tables/06_evidence_map_tables.xlsx"
  )

  write_json(
    list(
      module =
        "06_evidence_map",
      decision_context_cells =
        nrow(map_data),
      contexts =
        n_distinct(
          map_data$decision_context
        ),
      status = "PASS"
    ),
    "derived/evidence_map_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Publication-final evidence map completed."
  )
})

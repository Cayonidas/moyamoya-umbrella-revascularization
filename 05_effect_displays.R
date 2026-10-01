source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")
source("R/anchor_helpers.R")
suppressPackageStartupMessages(library(patchwork))

run_logged_module("05_effect_displays", {
  anchors <- safe_read_rds(
    "derived/anchor_effects.rds",
    "04_anchor_selection.R"
  )
  corroborators <- safe_read_rds(
    "derived/corroborator_effects.rds",
    "04_anchor_selection.R"
  )

  display_data <- bind_rows(
    anchors,
    corroborators
  ) |>
    add_conclusion_class() |>
    mutate(
      display_role = factor(
        anchor_role,
        levels = c(
          "Anchor",
          "Corroborator"
        )
      ),
      direction_group =
        conclusion_direction(
          publication_conclusion_class
        )
    )

  ratio_data <- display_data |>
    filter(
      measure_code %in%
        c(
          "EM_OR",
          "EM_RR",
          "EM_HR"
        ),
      !is.na(
        display_ci_lower
      ),
      display_ci_lower > 0
    )

  make_ratio_forest <- function(
    data,
    comparison,
    measure
  ) {
    dat <- data |>
      filter(
        comparison_id ==
          comparison,
        measure_code ==
          measure
      ) |>
      arrange(
        outcome_code,
        horizon_code,
        desc(
          display_role ==
            "Anchor"
        ),
        display_estimate
      ) |>
      mutate(
        row_label =
          paste0(
            outcome_label,
            " [",
            horizon_label,
            "] — ",
            `Review ID`
          ),
        row_label =
          factor(
            row_label,
            levels =
              rev(
                unique(
                  row_label
                )
              )
          )
      )

    if (!nrow(dat)) {
      return(NULL)
    }

    ggplot(
      dat,
      aes(
        x =
          display_estimate,
        y =
          row_label,
        colour =
          display_role,
        shape =
          display_role
      )
    ) +
      geom_vline(
        xintercept = 1,
        linetype = 2,
        colour =
          "#7F7F7F"
      ) +
      geom_segment(
        aes(
          x =
            display_ci_lower,
          xend =
            display_ci_upper,
          yend =
            row_label
        ),
        linewidth = 0.55
      ) +
      geom_point(
        size = 2.6
      ) +
      scale_x_log10() +
      scale_colour_manual(
        values = c(
          "Anchor" =
            "#17365D",
          "Corroborator" =
            "#7F8C8D"
        )
      ) +
      scale_shape_manual(
        values = c(
          "Anchor" = 18,
          "Corroborator" = 16
        )
      ) +
      labs(
        title =
          label_code(
            comparison,
            comparison_labels
          ),
        subtitle =
          paste0(
            label_code(
              measure,
              measure_labels
            ),
            " estimates in canonical comparison orientation; no umbrella-level pooling"
          ),
        x =
          paste0(
            label_code(
              measure,
              measure_labels
            ),
            " (log scale)"
          ),
        y = NULL,
        colour = NULL,
        shape = NULL,
        caption =
          "Diamonds identify review anchors; circles identify corroborators. No pooled umbrella estimate is calculated."
      ) +
      theme_moyamoya(9)
  }

  ratio_combinations <-
    ratio_data |>
    distinct(
      comparison_id,
      measure_code
    )

  if (
    nrow(
      ratio_combinations
    )
  ) {
    for (
      i in seq_len(
        nrow(
          ratio_combinations
        )
      )
    ) {
      comp <-
        ratio_combinations$comparison_id[[i]]
      meas <-
        ratio_combinations$measure_code[[i]]

      p <- make_ratio_forest(
        ratio_data,
        comp,
        meas
      )

      save_plot_multiformat(
        p,
        file.path(
          "results/figures/supplement",
          paste0(
            "Forest_",
            comp,
            "_",
            meas
          )
        ),
        9.0,
        max(
          4.6,
          2.7 +
            0.48 *
            nrow(
              ratio_data |>
                filter(
                  comparison_id ==
                    comp,
                  measure_code ==
                    meas
                )
            )
        )
      )
    }
  }

  continuous_data <-
    display_data |>
    filter(
      measure_code %in%
        c(
          "EM_MD",
          "EM_WMD"
        ),
      !is.na(
        display_estimate
      )
    )

  if (
    nrow(
      continuous_data
    )
  ) {
    continuous_data <-
      continuous_data |>
      mutate(
        row_label =
          factor(
            paste0(
              outcome_label,
              " — ",
              `Review ID`
            ),
            levels =
              rev(
                unique(
                  paste0(
                    outcome_label,
                    " — ",
                    `Review ID`
                  )
                )
              )
          )
      )

    p_cont <- ggplot(
      continuous_data,
      aes(
        x =
          display_estimate,
        y =
          row_label
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = 2,
        colour =
          "#7F7F7F"
      ) +
      geom_segment(
        aes(
          x =
            display_ci_lower,
          xend =
            display_ci_upper,
          yend =
            row_label
        ),
        linewidth = 0.6,
        colour =
          "#17365D"
      ) +
      geom_point(
        size = 2.7,
        colour =
          "#17365D"
      ) +
      labs(
        title =
          "Continuous functional outcomes",
        subtitle =
          "Canonical orientation; lower mRS is treated as better when appropriate",
        x =
          "Mean difference / weighted mean difference",
        y = NULL
      ) +
      theme_moyamoya(9)

    save_plot_multiformat(
      p_cont,
      "results/figures/supplement/Forest_continuous_outcomes",
      8.5,
      4.8
    )
  }

  main_ids <- c(
    "E0009","E0010","E0129","E0132",
    "E0012","E0013","E0295","E0261",
    "E0053","E0105","E0063","E0064","E0110",
    "E0057","E0106","E0065","E0066","E0067"
  )

  panel_map <- tibble(
    `Effect ID` =
      main_ids,
    Panel = c(
      rep(
        "A  Adult symptomatic: surgery vs conservative",
        4
      ),
      rep(
        "B  Hemorrhagic phenotype: revascularization",
        4
      ),
      rep(
        "C  Adult direct vs indirect",
        5
      ),
      rep(
        "D  Adult combined vs indirect",
        5
      )
    ),
    Row_order = c(
      1,2,3,4,
      1,2,3,4,
      1,2,3,4,5,
      1,2,3,4,5
    )
  )

  main_forest <- anchors |>
    inner_join(
      panel_map,
      by =
        "Effect ID"
    ) |>
    filter(
      ratio_measure,
      !is.na(
        display_ci_lower
      ),
      display_ci_lower > 0
    ) |>
    mutate(
      Panel =
        factor(
          Panel,
          levels =
            unique(
              panel_map$Panel
            )
        ),
      outcome_display =
        case_when(
          `Effect ID` %in%
            c(
              "E0129",
              "E0295"
            ) ~
              "Recurrent/any stroke",
          TRUE ~
            outcome_label
        ),
      direction_group =
        conclusion_direction(
          publication_conclusion_class
        ),
      row_label =
        paste0(
          outcome_display,
          " — ",
          measure_label,
          " ",
          sprintf(
            "%.2g",
            display_estimate
          ),
          " (",
          sprintf(
            "%.2g",
            display_ci_lower
          ),
          "–",
          sprintf(
            "%.2g",
            display_ci_upper
          ),
          ")"
        )
    ) |>
    arrange(
      Panel,
      Row_order
    ) |>
    group_by(
      Panel
    ) |>
    mutate(
      row_factor =
        factor(
          row_label,
          levels =
            rev(
              row_label
            )
        )
    ) |>
    ungroup()

  if (
    nrow(
      main_forest
    ) != 18L
  ) {
    stop(
      "Main Figure 3 requires exactly 18 prespecified anchor effects.",
      call. = FALSE
    )
  }

  p_main_forest <- ggplot(
    main_forest,
    aes(
      x =
        display_estimate,
      y =
        row_factor,
      colour =
        direction_group
    )
  ) +
    geom_vline(
      xintercept = 1,
      linetype = 2,
      colour =
        "#7F7F7F"
    ) +
    geom_segment(
      aes(
        x =
          display_ci_lower,
        xend =
          display_ci_upper,
        yend =
          row_factor
      ),
      linewidth = 0.65
    ) +
    geom_point(
      size = 2.9
    ) +
    scale_x_log10() +
    scale_colour_manual(
      values = c(
        "Favors intervention" =
          "#2E7D32",
        "Favors comparator" =
          "#C65D1E",
        "No clear difference" =
          "#7F8C8D",
        "Not directionally interpretable" =
          "#7F8C8D"
      )
    ) +
    facet_wrap(
      ~Panel,
      ncol = 2,
      scales =
        "free_y"
    ) +
    labs(
      title =
        "Figure 3. Principal review-anchor estimates for adult surgical decision-making",
      subtitle =
        "Canonical comparison orientation; event-specific early safety and late efficacy are displayed separately",
      x =
        "Effect estimate (log scale; null = 1)",
      y = NULL,
      colour = NULL,
      caption =
        "No umbrella-level pooling was performed. OR and RR estimates are labelled explicitly and should not be numerically combined."
    ) +
    theme_moyamoya(
      9.5
    ) +
    theme(
      strip.text =
        element_text(
          face =
            "bold",
          hjust = 0
        ),
      legend.position =
        "bottom",
      axis.text.y =
        element_text(
          size =
            8.2
        )
    )

  save_plot_multiformat(
    p_main_forest,
    "results/figures/main/Figure_3_anchor_review_forests",
    13.2,
    9.6
  )

  concordance <-
    display_data |>
    group_by(
      evidence_cell_id,
      phenotype_code,
      phenotype_label,
      comparison_id,
      comparison_label,
      outcome_code,
      outcome_label,
      horizon_code,
      horizon_label
    ) |>
    summarise(
      reviews =
        n_distinct(
          `Review ID`
        ),
      anchor_review =
        first(
          `Review ID`[
            display_role ==
              "Anchor"
          ]
        ),
      anchor_conclusion =
        first(
          publication_conclusion_class[
            display_role ==
              "Anchor"
          ]
        ),
      direction_classes =
        paste(
          sort(
            unique(
              conclusion_direction(
                publication_conclusion_class
              )
            )
          ),
          collapse =
            "; "
        ),
      direction_concordant =
        n_distinct(
          conclusion_direction(
            publication_conclusion_class
          )
        ) == 1,
      .groups = "drop"
    )

  outputs <- list(
    forest_display_data =
      display_data,
    main_forest_data =
      main_forest,
    ratio_forest_data =
      ratio_data,
    continuous_forest_data =
      continuous_data,
    direction_concordance =
      concordance
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
    "results/tables/05_effect_display_tables.xlsx"
  )

  write_json(
    list(
      module =
        "05_effect_displays",
      anchors_displayed =
        sum(
          display_data$display_role ==
            "Anchor"
        ),
      corroborators_displayed =
        sum(
          display_data$display_role ==
            "Corroborator"
        ),
      main_forest_rows =
        nrow(
          main_forest
        ),
      supplemental_ratio_panels =
        nrow(
          ratio_combinations
        ),
      status = "PASS"
    ),
    "derived/effect_display_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Publication-final effect displays completed."
  )
})

source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")
source("R/anchor_helpers.R")
suppressPackageStartupMessages(library(patchwork))

run_logged_module("07_tradeoff_translation", {
  anchors <- safe_read_rds(
    "derived/anchor_effects.rds",
    "04_anchor_selection.R"
  ) |>
    add_conclusion_class()

  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  ) |>
    filter(
      inference_action !=
        "exclude_duplicate"
    )

  tradeoff_ids <- tibble(
    `Effect ID` = c(
      "E0053","E0105","E0063","E0064","E0110",
      "E0057","E0106","E0065","E0066","E0067"
    ),
    Strategy = c(
      rep(
        "Direct vs indirect",
        5
      ),
      rep(
        "Combined vs indirect",
        5
      )
    ),
    Domain = rep(
      c(
        "Early ischemic stroke",
        "Perioperative hemorrhage",
        "Late ischemic stroke",
        "Late hemorrhage",
        "Favorable function"
      ),
      2
    )
  )

  tradeoff <- anchors |>
    inner_join(
      tradeoff_ids,
      by =
        "Effect ID"
    ) |>
    mutate(
      direction =
        conclusion_direction(
          publication_conclusion_class
        ),
      cell_text =
        paste0(
          measure_label,
          " ",
          if_else(
            display_estimate >= 10,
            sprintf(
              "%.1f",
              display_estimate
            ),
            sprintf(
              "%.2f",
              display_estimate
            )
          ),
          "\n(",
          if_else(
            display_ci_lower >= 10,
            sprintf(
              "%.1f",
              display_ci_lower
            ),
            sprintf(
              "%.2f",
              display_ci_lower
            )
          ),
          "–",
          if_else(
            display_ci_upper >= 10,
            sprintf(
              "%.1f",
              display_ci_upper
            ),
            sprintf(
              "%.2f",
              display_ci_upper
            )
          ),
          ")\n",
          case_when(
            direction ==
              "Favors intervention" ~
                paste0(
                  "favors ",
                  display_intervention
                ),
            direction ==
              "Favors comparator" ~
                paste0(
                  "favors ",
                  display_comparator
                ),
            TRUE ~
                "no clear difference"
          )
        ),
      Strategy =
        factor(
          Strategy,
          levels = c(
            "Combined vs indirect",
            "Direct vs indirect"
          )
        ),
      Domain =
        factor(
          Domain,
          levels = c(
            "Early ischemic stroke",
            "Perioperative hemorrhage",
            "Late ischemic stroke",
            "Late hemorrhage",
            "Favorable function"
          )
        )
    )

  if (
    nrow(
      tradeoff
    ) != 10L
  ) {
    stop(
      "Event-specific trade-off matrix requires exactly 10 prespecified anchor effects.",
      call. = FALSE
    )
  }

  p_tradeoff <- ggplot(
    tradeoff,
    aes(
      x =
        Domain,
      y =
        Strategy,
      fill =
        direction
    )
  ) +
    geom_tile(
      colour = "white",
      linewidth = 1.1
    ) +
    geom_text(
      aes(
        label =
          cell_text
      ),
      size = 3.15,
      lineheight = 0.93
    ) +
    scale_fill_manual(
      values = c(
        "Favors intervention" =
          "#D9EAD3",
        "Favors comparator" =
          "#FCE4D6",
        "No clear difference" =
          "#E7E6E6",
        "Not directionally interpretable" =
          "#E7E6E6"
      )
    ) +
    labs(
      title =
        "Figure 4. Event-specific trade-off between immediate-flow and indirect strategies",
      subtitle =
        "Adult comparative review anchors show that early ischemic and late clinical signals can favor direct/combined bypass while perioperative hemorrhage favors indirect bypass",
      x = NULL,
      y = NULL,
      fill = NULL,
      caption =
        "Effect estimates are shown in canonical row orientation. Early-hemorrhage ORs are reciprocals of source-reported indirect-versus-direct/combined estimates; no umbrella-level pooling was performed."
    ) +
    theme_moyamoya(10) +
    theme(
      axis.text.x =
        element_text(
          angle = 30,
          hjust = 1,
          face = "bold"
        ),
      axis.text.y =
        element_text(
          face = "bold"
        ),
      panel.grid =
        element_blank(),
      legend.position =
        "bottom"
    )

  save_plot_multiformat(
    p_tradeoff,
    "results/figures/main/Figure_4_early_late_tradeoff",
    12.8,
    5.8
  )

  stage_levels <- c(
    "Technical",
    "Angiographic",
    "Hemodynamic",
    "Clinical events/safety",
    "Functional",
    "Patient-centered"
  )

  stage_counts <- effects |>
    filter(
      effect_stage %in%
        stage_levels
    ) |>
    count(
      effect_stage,
      name =
        "Effect rows"
    ) |>
    complete(
      effect_stage =
        stage_levels,
      fill = list(
        `Effect rows` =
          0
      )
    ) |>
    mutate(
      effect_stage =
        factor(
          effect_stage,
          levels =
            stage_levels
        ),
      x =
        as.numeric(
          effect_stage
        ),
      y = 1
    )

  context_stage <- effects |>
    filter(
      comparison_id %in%
        c(
          "C01","C06","C02",
          "C03","C15","C05",
          "C09"
        ),
      effect_stage %in%
        stage_levels
    ) |>
    mutate(
      Context =
        case_when(
          comparison_id ==
            "C01" ~
            "Adult surgery vs conservative",
          comparison_id ==
            "C06" ~
            "Hemorrhagic revascularization",
          comparison_id ==
            "C02" ~
            "Direct vs indirect",
          comparison_id ==
            "C03" ~
            "Combined vs indirect",
          comparison_id ==
            "C15" ~
            "Direct/combined vs indirect",
          comparison_id ==
            "C05" ~
            "Pediatric techniques",
          comparison_id ==
            "C09" ~
            "Antiplatelet therapy"
        )
    ) |>
    count(
      Context,
      effect_stage,
      name =
        "Effect rows"
    ) |>
    mutate(
      effect_stage =
        factor(
          effect_stage,
          levels =
            stage_levels
        )
    )

  p_ladder <- ggplot(
    stage_counts,
    aes(
      x = x,
      y = y
    )
  ) +
    geom_segment(
      data =
        tibble(
          x = 1:5,
          xend = 2:6,
          y = 1,
          yend = 1
        ),
      aes(
        x = x,
        xend = xend,
        y = y,
        yend = yend
      ),
      linewidth = 0.7,
      colour =
        "#7F8C8D",
      arrow =
        grid::arrow(
          length =
            grid::unit(
              0.10,
              "inches"
            )
        )
    ) +
    geom_label(
      aes(
        label =
          paste0(
            effect_stage,
            "\n",
            `Effect rows`,
            " rows"
          ),
        fill =
          effect_stage ==
            "Patient-centered"
      ),
      colour =
        "#17365D",
      linewidth =
        0.4,
      fontface =
        "bold",
      size =
        3.0,
      label.padding =
        grid::unit(
          0.16,
          "lines"
        ),
      show.legend =
        FALSE
    ) +
    scale_fill_manual(
      values = c(
        `FALSE` =
          "#EAF3F8",
        `TRUE` =
          "#FCE4D6"
      )
    ) +
    coord_cartesian(
      xlim =
        c(0.55,6.45),
      ylim =
        c(0.75,1.25),
      clip =
        "off"
    ) +
    labs(
      title =
        "A. Evidence translation ladder",
      subtitle =
        "The literature is concentrated on events and technical/surrogate outcomes"
    ) +
    theme_void(
      base_size = 9
    ) +
    theme(
      plot.title =
        element_text(
          face = "bold",
          colour =
            "#17365D"
        ),
      plot.subtitle =
        element_text(
          colour =
            "#404040"
        )
    )

  p_context <- ggplot(
    context_stage,
    aes(
      x =
        effect_stage,
      y =
        Context,
      fill =
        `Effect rows`
    )
  ) +
    geom_tile(
      colour =
        "white",
      linewidth =
        0.5
    ) +
    geom_text(
      aes(
        label =
          ifelse(
            `Effect rows` >
              0,
            `Effect rows`,
            ""
          )
      ),
      size = 3
    ) +
    scale_fill_gradient(
      low =
        "#EEF5FA",
      high =
        "#17365D"
    ) +
    labs(
      title =
        "B. Translation depth across key decisions",
      x = NULL,
      y = NULL,
      fill =
        "Effect rows"
    ) +
    theme_moyamoya(
      8.5
    ) +
    theme(
      axis.text.x =
        element_text(
          angle = 40,
          hjust = 1
        ),
      panel.grid =
        element_blank()
    )

  p_translation <-
    p_ladder /
    p_context +
    plot_annotation(
      title =
        "Figure 5. From technical success to patient-important recovery",
      subtitle =
        "Angiographic and hemodynamic signals should not be interpreted as proven surrogates for stroke prevention, function, cognition, or quality of life",
      theme =
        theme(
          plot.title =
            element_text(
              face = "bold",
              colour =
                "#17365D",
              size = 14
            )
        )
    )

  save_plot_multiformat(
    p_translation,
    "results/figures/main/Figure_5_translation_chain",
    13.4,
    8.7
  )

  outputs <- list(
    event_specific_tradeoff =
      tradeoff,
    translation_stage_counts =
      stage_counts,
    translation_context_stage =
      context_stage
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
    "results/tables/07_tradeoff_translation_tables.xlsx"
  )

  write_json(
    list(
      module =
        "07_tradeoff_translation",
      tradeoff_cells =
        nrow(
          tradeoff
        ),
      patient_centered_effect_rows =
        stage_counts$`Effect rows`[
          stage_counts$effect_stage ==
            "Patient-centered"
        ],
      status = "PASS"
    ),
    "derived/tradeoff_translation_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Publication-final trade-off and translation module completed."
  )
})

source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("03_descriptive_reviews", {
  review_master <- safe_read_rds(
    "derived/review_master_validated.rds",
    "01_import_validate.R"
  )
  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  )

  flow <- fromJSON(
    "config/study_flow.json",
    simplifyVector = TRUE
  )

  flow_table <- tibble(
    Stage = c(
      "Unique records screened",
      "Excluded at title/abstract",
      "Tracked separately",
      "Reports sought for retrieval",
      "Full texts unavailable",
      "Conference/annals abstracts",
      "Reports assessed in full text",
      "Formal umbrella-review units",
      "Context-only reports",
      "Editorial notices/corrigenda discovered"
    ),
    Count = c(
      flow$unique_records_screened,
      flow$title_abstract_excluded,
      flow$tracked_separately,
      flow$reports_sought_for_retrieval,
      flow$full_text_unavailable,
      flow$conference_annals_abstracts,
      flow$reports_assessed_full_text,
      flow$formal_umbrella_units,
      flow$context_only_reports,
      flow$editorial_notices_discovered
    )
  )

  main_boxes <- tibble(
    x = c(0,0,0,-0.85,0.85),
    y = c(4.8,3.55,2.30,0.90,0.90),
    label = c(
      sprintf(
        "Unique records screened\n(n = %d)",
        flow$unique_records_screened
      ),
      sprintf(
        "Reports sought for full-text retrieval\n(n = %d)",
        flow$reports_sought_for_retrieval
      ),
      sprintf(
        "Full-text reports assessed\n(n = %d)",
        flow$reports_assessed_full_text
      ),
      sprintf(
        "Formal umbrella-review units\n(n = %d)",
        flow$formal_umbrella_units
      ),
      sprintf(
        "Context-only reports\n(n = %d)",
        flow$context_only_reports
      )
    ),
    type = c("main","main","main","formal","context")
  )

  side_boxes <- tibble(
    x = c(2.45,2.45,2.45),
    y = c(4.8,3.55,2.30),
    label = c(
      sprintf(
        "Title/abstract excluded: %d\nTracked separately: %d",
        flow$title_abstract_excluded,
        flow$tracked_separately
      ),
      sprintf(
        "Not retrieved as eligible full text: %d\nUnavailable full text: %d\nConference/annals abstract only: %d",
        flow$full_text_unavailable +
          flow$conference_annals_abstracts,
        flow$full_text_unavailable,
        flow$conference_annals_abstracts
      ),
      sprintf(
        "Editorial notices/corrigenda discovered within retrieved PDFs: %d\nNot independent screened evidence units",
        flow$editorial_notices_discovered
      )
    )
  )

  p_prisma <- ggplot() +
    geom_segment(
      data = tibble(
        x = c(0,0),
        y = c(4.40,3.15),
        xend = c(0,0),
        yend = c(3.95,2.70)
      ),
      aes(x=x,y=y,xend=xend,yend=yend),
      linewidth = 0.6,
      colour = "#5B6573",
      arrow = grid::arrow(
        length = grid::unit(0.12, "inches")
      )
    ) +
    geom_segment(
      data = tibble(
        x = c(0.70,0.70,0.70),
        y = c(4.8,3.55,2.30),
        xend = c(1.65,1.65,1.65),
        yend = c(4.8,3.55,2.30)
      ),
      aes(x=x,y=y,xend=xend,yend=yend),
      linewidth = 0.45,
      colour = "#7F8C8D",
      arrow = grid::arrow(
        length = grid::unit(0.10, "inches")
      )
    ) +
    geom_segment(
      data = tibble(
        x = c(0,0),
        y = c(1.88,1.88),
        xend = c(-0.85,0.85),
        yend = c(1.32,1.32)
      ),
      aes(x=x,y=y,xend=xend,yend=yend),
      linewidth = 0.6,
      colour = "#5B6573",
      arrow = grid::arrow(
        length = grid::unit(0.12, "inches")
      )
    ) +
    geom_label(
      data = main_boxes,
      aes(x=x,y=y,label=label,fill=type),
      size = 3.5,
      linewidth = 0.45,
      colour = "#17365D",
      fontface = "bold",
      label.padding = grid::unit(0.22, "lines"),
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = c(
        main = "#EAF3F8",
        formal = "#D9EAD3",
        context = "#F2F2F2"
      )
    ) +
    geom_label(
      data = side_boxes,
      aes(x=x,y=y,label=label),
      size = 2.9,
      linewidth = 0.35,
      fill = "#F7F7F7",
      colour = "#404040",
      label.padding = grid::unit(0.18, "lines")
    ) +
    coord_cartesian(
      xlim = c(-1.75,3.55),
      ylim = c(0.25,5.35),
      clip = "off"
    ) +
    labs(
      title =
        "Figure 1. Study selection and full-text disposition",
      subtitle =
        "Consensus title/abstract screening followed by full-text eligibility for the formal umbrella synthesis",
      caption =
        "Context-only reports are parallel to, not downstream of, the 68 formal umbrella-review units. Editorial notices/corrigenda were discovered within retrieved PDFs and were not independent screened evidence units."
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(
        face="bold",
        colour="#17365D",
        hjust=0.5,
        size=13
      ),
      plot.subtitle = element_text(
        hjust=0.5,
        colour="#404040"
      ),
      plot.caption = element_text(
        colour="#666666",
        size=8
      ),
      plot.margin = margin(15,20,15,20)
    )

  save_plot_multiformat(
    p_prisma,
    "results/figures/main/Figure_1_PRISMA",
    10.5,
    7.9
  )

  formal_reviews <- review_master |>
    filter(`Formal umbrella unit` == "Yes") |>
    mutate(
      Year = suppressWarnings(as.numeric(Year)),
      publication_period = case_when(
        Year <= 2019 ~ "≤2019",
        Year <= 2023 ~ "2020–2023",
        TRUE ~ "2024–2026"
      ),
      tier_short = if_else(
        str_starts(`Master tier`, "Tier 1"),
        "Tier 1 core",
        "Tier 2 contextual"
      ),
      meta_analysis =
        yes_like(`Meta-analysis performed`),
      protocol =
        yes_like(`Registration/protocol`),
      duplicate_selection =
        yes_like(`Selection in duplicate`),
      duplicate_extraction =
        yes_like(`Extraction in duplicate`),
      risk_of_bias = !str_detect(
        str_to_lower(
          coalesce(`Risk-of-bias tool`, "")
        ),
        "^no|no formal|^nr$|^na$"
      ),
      grade =
        yes_like(`GRADE/certainty`),
      hybrid =
        str_to_lower(
          coalesce(`Hybrid report`, "")
        ) != "no"
    )

  table1 <- formal_reviews |>
    transmute(
      `Review ID`,
      `Screening ID`,
      Title,
      Authors,
      Year,
      Journal,
      Tier = tier_short,
      `Evidence domain`,
      `Review type` =
        `Master review type`,
      Population =
        `Population and age`,
      `MMD/MMS status`,
      `Clinical presentation`,
      Intervention,
      Comparator,
      `Studies/reports`,
      Patients,
      Hemispheres,
      `Procedures/bypasses`,
      `Final search date`,
      `Meta-analysis performed`,
      `Master AMSTAR 2 overall`,
      `Master ROBIS overall`,
      `Outcome-table readiness`
    ) |>
    arrange(
      desc(Year),
      `Review ID`
    )

  methods_summary <- bind_rows(
    formal_reviews |> summarise(
      Characteristic = "Protocol/registration",
      `n/N` = sprintf(
        "%d/%d",
        sum(protocol),
        n()
      ),
      Percent = mean(protocol) * 100
    ),
    formal_reviews |> summarise(
      Characteristic = "Duplicate study selection",
      `n/N` = sprintf(
        "%d/%d",
        sum(duplicate_selection),
        n()
      ),
      Percent =
        mean(duplicate_selection) * 100
    ),
    formal_reviews |> summarise(
      Characteristic = "Duplicate data extraction",
      `n/N` = sprintf(
        "%d/%d",
        sum(duplicate_extraction),
        n()
      ),
      Percent =
        mean(duplicate_extraction) * 100
    ),
    formal_reviews |> summarise(
      Characteristic =
        "Formal primary-study risk-of-bias assessment",
      `n/N` = sprintf(
        "%d/%d",
        sum(risk_of_bias),
        n()
      ),
      Percent =
        mean(risk_of_bias) * 100
    ),
    formal_reviews |> summarise(
      Characteristic =
        "GRADE/certainty assessment",
      `n/N` = sprintf(
        "%d/%d",
        sum(grade),
        n()
      ),
      Percent =
        mean(grade) * 100
    ),
    formal_reviews |> summarise(
      Characteristic =
        "Meta-analysis performed",
      `n/N` = sprintf(
        "%d/%d",
        sum(meta_analysis),
        n()
      ),
      Percent =
        mean(meta_analysis) * 100
    ),
    formal_reviews |> summarise(
      Characteristic =
        "Hybrid review/institutional report",
      `n/N` = sprintf(
        "%d/%d",
        sum(hybrid),
        n()
      ),
      Percent =
        mean(hybrid) * 100
    )
  )

  review_counts <- formal_reviews |>
    count(
      publication_period,
      tier_short,
      name = "Reviews"
    )

  p_timeline <- ggplot(
    review_counts,
    aes(
      publication_period,
      Reviews,
      fill = tier_short
    )
  ) +
    geom_col(
      position = "stack",
      width = 0.72
    ) +
    scale_fill_manual(
      values = c(
        "Tier 1 core" = "#2F75B5",
        "Tier 2 contextual" = "#9DC3E6"
      )
    ) +
    labs(
      title =
        "Temporal growth of the formal moyamoya review literature",
      x = "Publication period",
      y = "Number of reviews",
      fill = NULL
    ) +
    theme_moyamoya(11)

  save_plot_multiformat(
    p_timeline,
    "results/figures/supplement/Figure_S3_review_timeline",
    7.2,
    4.8
  )

  quality_counts <- formal_reviews |>
    count(
      `Master AMSTAR 2 overall`,
      `Master ROBIS overall`,
      name = "Reviews"
    )

  p_quality <- ggplot(
    quality_counts,
    aes(
      `Master AMSTAR 2 overall`,
      Reviews,
      fill = `Master ROBIS overall`
    )
  ) +
    geom_col(
      position = "stack",
      width = 0.72
    ) +
    scale_fill_manual(
      values = c(
        "High" = "#C00000",
        "Unclear" = "#FFD966",
        "Low" = "#70AD47"
      )
    ) +
    labs(
      title =
        "Methodological confidence of the included reviews",
      x = "AMSTAR 2 overall confidence",
      y = "Number of reviews",
      fill = "ROBIS overall"
    ) +
    theme_moyamoya(11)

  save_plot_multiformat(
    p_quality,
    "results/figures/supplement/Figure_S_quality_architecture",
    7.2,
    4.8
  )

  outcome_stage <- effects |>
    filter(
      inference_action !=
        "exclude_duplicate"
    ) |>
    count(
      effect_stage,
      name = "Effect rows"
    ) |>
    mutate(
      effect_stage = factor(
        effect_stage,
        levels = c(
          "Technical",
          "Angiographic",
          "Hemodynamic",
          "Clinical events/safety",
          "Functional",
          "Patient-centered",
          "Other/composite"
        )
      ),
      Percent =
        `Effect rows` /
        sum(`Effect rows`) *
        100
    )

  p_outcomes <- ggplot(
    outcome_stage,
    aes(
      fct_reorder(
        effect_stage,
        `Effect rows`
      ),
      `Effect rows`
    )
  ) +
    geom_col(
      width = 0.72,
      fill = "#2F75B5"
    ) +
    coord_flip() +
    geom_text(
      aes(
        label = sprintf(
          "%d (%.1f%%)",
          `Effect rows`,
          Percent
        )
      ),
      hjust = -0.08,
      size = 3.2
    ) +
    expand_limits(
      y = max(
        outcome_stage$`Effect rows`
      ) * 1.18
    ) +
    labs(
      title =
        "Outcome architecture of the evidence base",
      subtitle =
        "Technical and event-based outcomes dominate over lived recovery outcomes",
      x = NULL,
      y = "Harmonized effect rows"
    ) +
    theme_moyamoya(11)

  save_plot_multiformat(
    p_outcomes,
    "results/figures/supplement/Figure_S_outcome_imbalance",
    8.0,
    5.2
  )

  domain_summary <- formal_reviews |>
    count(
      tier_short,
      `Evidence domain`,
      sort = TRUE,
      name = "Reviews"
    )

  table5_methods <- formal_reviews |>
    summarise(
      `Formal reviews` = n(),
      `Tier 1 core` =
        sum(tier_short == "Tier 1 core"),
      `Tier 2 contextual` =
        sum(tier_short == "Tier 2 contextual"),
      `AMSTAR 2 Low` =
        sum(`Master AMSTAR 2 overall` == "Low"),
      `AMSTAR 2 Critically low` =
        sum(
          `Master AMSTAR 2 overall` ==
            "Critically low"
        ),
      `ROBIS High` =
        sum(`Master ROBIS overall` == "High"),
      `ROBIS Unclear` =
        sum(
          `Master ROBIS overall` ==
            "Unclear"
        ),
      `Registered/protocolized` =
        sum(protocol),
      `Duplicate selection` =
        sum(duplicate_selection),
      `Duplicate extraction` =
        sum(duplicate_extraction),
      `Formal risk-of-bias assessment` =
        sum(risk_of_bias),
      `GRADE/certainty` =
        sum(grade)
    ) |>
    pivot_longer(
      everything(),
      names_to =
        "Methodological feature",
      values_to =
        "Count"
    )

  outputs <- list(
    Table1_review_characteristics = table1,
    Table5_methodological_summary =
      table5_methods,
    Methods_completeness =
      methods_summary,
    Evidence_domain_summary =
      domain_summary,
    Outcome_stage_summary =
      outcome_stage,
    Formal_reviews_analysis =
      formal_reviews,
    PRISMA_flow =
      flow_table
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
    outputs,
    "results/tables/03_descriptive_review_tables.xlsx"
  )

  write_json(
    list(
      module =
        "03_descriptive_reviews",
      formal_reviews =
        nrow(formal_reviews),
      tier1 =
        sum(
          formal_reviews$tier_short ==
            "Tier 1 core"
        ),
      tier2 =
        sum(
          formal_reviews$tier_short ==
            "Tier 2 contextual"
        ),
      amstar_low =
        sum(
          formal_reviews$`Master AMSTAR 2 overall` ==
            "Low"
        ),
      amstar_critically_low =
        sum(
          formal_reviews$`Master AMSTAR 2 overall` ==
            "Critically low"
        ),
      robis_high =
        sum(
          formal_reviews$`Master ROBIS overall` ==
            "High"
        ),
      robis_unclear =
        sum(
          formal_reviews$`Master ROBIS overall` ==
            "Unclear"
        ),
      patient_centered_effect_rows =
        sum(
          effects$effect_stage ==
            "Patient-centered"
        ),
      status = "PASS"
    ),
    "derived/descriptive_reviews_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Descriptive review module completed."
  )
})

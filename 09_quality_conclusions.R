source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("09_quality_conclusions", {
  master_path <- "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx"
  reviews <- safe_read_rds(
    "derived/review_master_validated.rds", "01_import_validate.R"
  ) |>
    filter(`Formal umbrella unit` == "Yes") |>
    mutate(
      Year = suppressWarnings(as.numeric(Year)),
      publication_period = case_when(
        Year <= 2019 ~ "≤2019",
        Year <= 2023 ~ "2020–2023",
        TRUE ~ "2024–2026"
      )
    )

  amstar_items <- read_excel(master_path, sheet = "AMSTAR 2 items") |>
    filter(`Review ID` %in% reviews$`Review ID`)
  robis_items <- read_excel(master_path, sheet = "ROBIS items") |>
    filter(`Review ID` %in% reviews$`Review ID`)

  review_order <- reviews |>
    arrange(`Master tier`, Year, `Review ID`) |>
    pull(`Review ID`)

  amstar_plot_data <- amstar_items |>
    mutate(
      `Review ID` = factor(`Review ID`, levels = review_order),
      Item = factor(Item, levels = as.character(1:16)),
      `Master judgment` = factor(
        `Master judgment`,
        levels = c("Yes","Partial Yes","No","Not applicable")
      )
    )

  p_amstar <- ggplot(
    amstar_plot_data,
    aes(Item, `Review ID`, fill = `Master judgment`)
  ) +
    geom_tile(colour = "white", linewidth = 0.15) +
    scale_fill_manual(
      values = c(
        "Yes" = "#70AD47",
        "Partial Yes" = "#FFD966",
        "No" = "#C00000",
        "Not applicable" = "#D9D9D9"
      ),
      drop = FALSE
    ) +
    labs(
      title = "AMSTAR 2 item-level profile",
      x = "AMSTAR 2 item", y = "Review", fill = NULL
    ) +
    theme_moyamoya(7) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 5)
    )
  save_plot_multiformat(
    p_amstar,
    "results/figures/supplement/Figure_S_AMSTAR2_heatmap",
    10.5, 16
  )

  robis_plot_data <- robis_items |>
    mutate(
      `Review ID` = factor(`Review ID`, levels = review_order),
      Domain = factor(Domain, levels = c("D1","D2","D3","D4","Overall")),
      `Master judgment` = factor(
        `Master judgment`, levels = c("Low","Unclear","High")
      )
    )

  p_robis <- ggplot(
    robis_plot_data,
    aes(Domain, `Review ID`, fill = `Master judgment`)
  ) +
    geom_tile(colour = "white", linewidth = 0.15) +
    scale_fill_manual(
      values = c(
        "Low" = "#70AD47",
        "Unclear" = "#FFD966",
        "High" = "#C00000"
      ),
      drop = FALSE
    ) +
    labs(
      title = "ROBIS domain-level risk of bias",
      x = "ROBIS domain", y = "Review", fill = NULL
    ) +
    theme_moyamoya(7) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 5)
    )
  save_plot_multiformat(
    p_robis,
    "results/figures/supplement/Figure_S_ROBIS_heatmap",
    7.8, 16
  )

  conclusion_strength <- reviews |>
    mutate(
      conclusion_text = str_squish(paste(
        coalesce(`Most supported benefit`, ""),
        coalesce(`Clinical decision informed`, ""),
        coalesce(`Conclusion not supported`, "")
      )),
      conclusion_strength = case_when(
        str_detect(
          str_to_lower(conclusion_text),
          "cannot|uncertain|insufficient|no clear|not supported|nr\\."
        ) &
          !str_detect(
            str_to_lower(conclusion_text),
            "superior|significant benefit|favou?rs|effective"
          ) ~ "Uncertain/nonrecommendatory",
        str_detect(
          str_to_lower(conclusion_text),
          "may|possible|associated|suggest|often|selected|conditional"
        ) ~ "Conditional",
        str_detect(
          str_to_lower(conclusion_text),
          "superior|best|significant benefit|favou?rs|effective|reduces"
        ) ~ "Strong",
        TRUE ~ "Unclear/not codable"
      ),
      strength_binary = conclusion_strength %in% c("Strong","Conditional"),
      amstar_binary = `Master AMSTAR 2 overall` == "Low",
      robis_binary = `Master ROBIS overall` == "Unclear",
      protocol = yes_like(`Registration/protocol`),
      duplicate_selection = yes_like(`Selection in duplicate`),
      duplicate_extraction = yes_like(`Extraction in duplicate`),
      risk_of_bias = !str_detect(
        str_to_lower(coalesce(`Risk-of-bias tool`, "")),
        "^no|no formal|^nr$|^na$"
      ),
      grade = yes_like(`GRADE/certainty`)
    )

  conclusion_table <- conclusion_strength |>
    count(
      `Master AMSTAR 2 overall`,
      `Master ROBIS overall`,
      conclusion_strength,
      name = "Reviews"
    )

  fisher_amstar <- tryCatch(
    fisher.test(table(
      conclusion_strength$strength_binary,
      conclusion_strength$amstar_binary
    )),
    error = function(e) NULL
  )
  fisher_robis <- tryCatch(
    fisher.test(table(
      conclusion_strength$strength_binary,
      conclusion_strength$robis_binary
    )),
    error = function(e) NULL
  )

  conclusion_tests <- tibble(
    Test = c(
      "Conclusion strength versus AMSTAR 2 Low",
      "Conclusion strength versus ROBIS Unclear"
    ),
    `Odds ratio` = c(
      if (is.null(fisher_amstar) || is.null(fisher_amstar$estimate)) {
        NA_real_
      } else {
        as.numeric(fisher_amstar$estimate)
      },
      if (is.null(fisher_robis) || is.null(fisher_robis$estimate)) {
        NA_real_
      } else {
        as.numeric(fisher_robis$estimate)
      }
    ),
    `p value` = c(
      if (is.null(fisher_amstar)) NA_real_ else fisher_amstar$p.value,
      if (is.null(fisher_robis)) NA_real_ else fisher_robis$p.value
    ),
    Interpretation = "Exploratory ecological review-level association; not causal."
  )

  methods_over_time <- conclusion_strength |>
    group_by(publication_period) |>
    summarise(
      Reviews = n(),
      Protocol = mean(protocol) * 100,
      `Duplicate selection` = mean(duplicate_selection) * 100,
      `Duplicate extraction` = mean(duplicate_extraction) * 100,
      `Risk-of-bias assessment` = mean(risk_of_bias) * 100,
      GRADE = mean(grade) * 100,
      .groups = "drop"
    ) |>
    pivot_longer(
      -c(publication_period, Reviews),
      names_to = "Method", values_to = "Percent"
    )

  p_time <- ggplot(
    methods_over_time,
    aes(publication_period, Percent, group = Method, colour = Method)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.4) +
    scale_y_continuous(labels = label_percent(scale = 1), limits = c(0,100)) +
    labs(
      title = "Evolution of review methods over time",
      x = "Publication period", y = "Reviews reporting the method",
      colour = NULL
    ) +
    theme_moyamoya(10)
  save_plot_multiformat(
    p_time,
    "results/figures/supplement/Figure_S_methods_over_time",
    9.5, 6
  )

  p_conclusion <- ggplot(
    conclusion_table,
    aes(
      `Master AMSTAR 2 overall`, Reviews,
      fill = conclusion_strength
    )
  ) +
    geom_col(position = "fill") +
    facet_wrap(~ `Master ROBIS overall`) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_manual(values = c(
      "Strong" = "#C00000",
      "Conditional" = "#ED7D31",
      "Uncertain/nonrecommendatory" = "#70AD47",
      "Unclear/not codable" = "#B4C6E7"
    )) +
    labs(
      title = "Strength of reconciled review conclusions versus methodological confidence",
      subtitle = "Exploratory coding based on reconciled conclusion fields, not a causal analysis",
      x = "AMSTAR 2", y = "Proportion of reviews", fill = NULL
    ) +
    theme_moyamoya(10)
  save_plot_multiformat(
    p_conclusion,
    "results/figures/supplement/Figure_S_conclusion_strength",
    9, 5.5
  )

  critical_item_summary <- amstar_items |>
    filter(`Critical domain` == "Yes") |>
    count(Item, `AMSTAR 2 item`, `Master judgment`, name = "Reviews") |>
    group_by(Item, `AMSTAR 2 item`) |>
    mutate(Percent = Reviews / sum(Reviews) * 100) |>
    ungroup()

  outputs <- list(
    conclusion_strength = conclusion_strength,
    conclusion_quality_crosstab = conclusion_table,
    conclusion_exploratory_tests = conclusion_tests,
    methods_over_time = methods_over_time,
    AMSTAR2_critical_item_summary = critical_item_summary,
    AMSTAR2_items = amstar_items,
    ROBIS_items = robis_items
  )
  walk2(outputs, names(outputs), ~ write_csv_and_rds(.x, file.path("derived", .y)))
  safe_write_xlsx(
    outputs,
    "results/tables/09_quality_conclusion_tables.xlsx"
  )
  write_json(
    list(
      module = "09_quality_conclusions",
      reviews = nrow(reviews),
      strong_conclusions =
        sum(conclusion_strength$conclusion_strength == "Strong"),
      conditional_conclusions =
        sum(conclusion_strength$conclusion_strength == "Conditional"),
      uncertain_conclusions =
        sum(conclusion_strength$conclusion_strength ==
              "Uncertain/nonrecommendatory"),
      status = "PASS"
    ),
    "derived/quality_conclusion_summary.json",
    pretty = TRUE, auto_unbox = TRUE
  )
  message("Quality and conclusion module completed.")
})

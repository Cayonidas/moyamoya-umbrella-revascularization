suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
})

analysis_type_rank <- function(x) {
  x <- str_to_lower(coalesce(as.character(x), ""))
  case_when(
    str_detect(
      x,
      "direct-comparison|comparative meta-analysis|fixed-effect meta-analysis|rct meta-analysis"
    ) ~ 6L,
    str_detect(x, "network meta-analysis") ~ 5L,
    str_detect(x, "meta-analysis") ~ 5L,
    str_detect(x, "pooled analysis/direct-comparison") ~ 5L,
    str_detect(x, "pooled analysis") ~ 3L,
    str_detect(x, "pooled comparison") ~ 2L,
    TRUE ~ 1L
  )
}

source_verification_rank <- function(x) {
  x <- str_to_lower(coalesce(as.character(x), ""))
  case_when(
    str_detect(x, "source-verified") ~ 3L,
    str_detect(x, "denominator audit") ~ 2L,
    TRUE ~ 1L
  )
}

prepare_anchor_candidates <- function(effects, review_master) {
  require_columns(
    effects,
    c(
      "Effect ID","Review ID","evidence_cell_id",
      "forest_final_eligible","corroborator_eligible",
      "analysis_type","source_verification",
      "studies_n","phenotype_code","unit_code",
      "Master AMSTAR 2","Master ROBIS",
      "ci_parse_valid","ratio_measure","continuous_measure",
      "ci_lower_numeric","ci_upper_numeric",
      "mapping_confidence"
    ),
    "effects_refined"
  )

  require_columns(
    review_master,
    c(
      "Review ID","Year","Final search date",
      "Studies/reports","Risk-of-bias tool",
      "Selection in duplicate","Extraction in duplicate",
      "Registration/protocol","Hybrid report",
      "Editorial family/update","Region",
      "Countries/centers","Conclusion not supported",
      "Priority research gap","Most supported benefit"
    ),
    "review_master_validated"
  )

  review_metrics <- review_master |>
    transmute(
      `Review ID`,
      review_year = suppressWarnings(as.numeric(Year)),
      search_year =
        parse_last_year_vec(`Final search date`, Year),
      review_studies =
        parse_study_count_vec(`Studies/reports`),
      rob_method_present = !str_detect(
        str_to_lower(
          coalesce(
            as.character(`Risk-of-bias tool`), ""
          )
        ),
        "^no|no formal|^nr$|^na$"
      ),
      duplicate_selection =
        coalesce(
          yes_like(`Selection in duplicate`),
          FALSE
        ),
      duplicate_extraction =
        coalesce(
          yes_like(`Extraction in duplicate`),
          FALSE
        ),
      registered_protocol =
        coalesce(
          yes_like(`Registration/protocol`),
          FALSE
        ),
      pure_review =
        str_to_lower(
          coalesce(as.character(`Hybrid report`), "")
        ) == "no",
      editorial_family =
        coalesce(as.character(`Editorial family/update`), ""),
      review_region =
        coalesce(as.character(Region), ""),
      review_countries =
        coalesce(as.character(`Countries/centers`), ""),
      conclusion_not_supported =
        coalesce(as.character(`Conclusion not supported`), ""),
      priority_gap =
        coalesce(as.character(`Priority research gap`), ""),
      most_supported_benefit =
        coalesce(as.character(`Most supported benefit`), "")
    )

  candidates <- effects |>
    filter(
      coalesce(forest_final_eligible, FALSE),
      coalesce(corroborator_eligible, FALSE)
    ) |>
    left_join(review_metrics, by = "Review ID") |>
    mutate(
      analysis_rank =
        analysis_type_rank(analysis_type),
      source_rank =
        source_verification_rank(source_verification),
      population_separable =
        phenotype_code != "MIXED_UNSEP",
      relevant_studies =
        parse_study_count_vec(studies_n),
      relevant_studies =
        coalesce(relevant_studies, review_studies, 0),
      estimate_complete =
        coalesce(ci_parse_valid, FALSE),
      unit_integrity =
        unit_code != "U07",
      amstar_rank =
        quality_rank_amstar(`Master AMSTAR 2`),
      robis_rank =
        quality_rank_robis(`Master ROBIS`),
      ci_width = case_when(
        ratio_measure &
          !is.na(ci_lower_numeric) &
          !is.na(ci_upper_numeric) &
          ci_lower_numeric > 0 &
          ci_upper_numeric > 0 ~
          safe_log_positive(ci_upper_numeric) -
          safe_log_positive(ci_lower_numeric),
        continuous_measure &
          !is.na(ci_lower_numeric) &
          !is.na(ci_upper_numeric) ~
          ci_upper_numeric - ci_lower_numeric,
        TRUE ~ Inf
      ),
      mapping_rank = case_when(
        mapping_confidence == "High" ~ 3L,
        mapping_confidence == "Moderate" ~ 2L,
        mapping_confidence == "Low" ~ 1L,
        TRUE ~ 0L
      )
    )

  candidates
}

read_manual_anchor_overrides <- function(path) {
  if (!file.exists(path)) return(tibble())

  lines <- readLines(
    path, warn = FALSE, encoding = "UTF-8"
  )
  if (length(lines) <= 1L) return(tibble())

  data_lines <- lines[-1]
  data_lines <- data_lines[nzchar(trimws(data_lines))]
  if (!length(data_lines)) return(tibble())

  read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA")
  )
}

select_anchors <- function(
  effects,
  review_master,
  manual_override_path =
    "config/anchor_manual_overrides.csv"
) {
  candidates <- prepare_anchor_candidates(
    effects, review_master
  )

  if (!nrow(candidates)) {
    return(tibble())
  }

  review_best <- candidates |>
    arrange(
      evidence_cell_id, `Review ID`,
      desc(analysis_rank),
      desc(source_rank),
      desc(estimate_complete),
      ci_width,
      desc(mapping_rank),
      `Effect ID`
    ) |>
    group_by(evidence_cell_id, `Review ID`) |>
    slice(1L) |>
    ungroup()

  ranked <- review_best |>
    arrange(
      evidence_cell_id,
      desc(analysis_rank),
      desc(population_separable),
      desc(search_year),
      desc(relevant_studies),
      desc(source_rank),
      desc(estimate_complete),
      desc(rob_method_present),
      desc(duplicate_selection),
      desc(duplicate_extraction),
      desc(registered_protocol),
      desc(unit_integrity),
      desc(amstar_rank),
      desc(robis_rank),
      desc(pure_review),
      ci_width,
      desc(mapping_rank),
      `Review ID`,
      `Effect ID`
    ) |>
    group_by(evidence_cell_id) |>
    mutate(
      algorithm_rank = row_number(),
      anchor_role = case_when(
        algorithm_rank == 1L ~ "Anchor",
        algorithm_rank <= 3L ~ "Corroborator",
        TRUE ~ "Additional eligible review"
      )
    ) |>
    ungroup()

  overrides <- read_manual_anchor_overrides(
    manual_override_path
  )

  if (nrow(overrides)) {
    require_columns(
      overrides,
      c("evidence_cell_id","Anchor Effect ID"),
      "anchor_manual_overrides"
    )

    valid_overrides <- overrides |>
      inner_join(
        ranked |>
          select(
            evidence_cell_id,
            `Effect ID`
          ),
        by = c(
          "evidence_cell_id",
          "Anchor Effect ID" = "Effect ID"
        )
      )

    invalid <- anti_join(
      overrides,
      valid_overrides,
      by = c(
        "evidence_cell_id",
        "Anchor Effect ID"
      )
    )

    if (nrow(invalid)) {
      stop(
        "Manual anchor override contains ineligible effect(s): ",
        paste(
          invalid$`Anchor Effect ID`,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    ranked <- ranked |>
      left_join(
        valid_overrides |>
          select(
            evidence_cell_id,
            manual_anchor_effect =
              `Anchor Effect ID`
          ),
        by = "evidence_cell_id"
      ) |>
      group_by(evidence_cell_id) |>
      arrange(
        desc(`Effect ID` == manual_anchor_effect),
        algorithm_rank,
        .by_group = TRUE
      ) |>
      mutate(
        final_rank = row_number(),
        anchor_role = case_when(
          final_rank == 1L ~ "Anchor",
          final_rank <= 3L ~ "Corroborator",
          TRUE ~ "Additional eligible review"
        ),
        manual_anchor_override =
          !is.na(manual_anchor_effect)
      ) |>
      ungroup()
  } else {
    ranked <- ranked |>
      mutate(
        final_rank = algorithm_rank,
        manual_anchor_override = FALSE
      )
  }

  ranked
}

add_conclusion_class <- function(data) {
  if (!nrow(data)) return(data)

  out <- canonicalize_effect_display(data)

  out |>
    mutate(
      conclusion_class = publication_conclusion_class,
      precision_class = publication_precision_class,
      benefit_axis_log_effect = if_else(
        ratio_measure,
        benefit_axis_effect,
        NA_real_
      ),
      benefit_ci_lower = benefit_ci_lower_final,
      benefit_ci_upper = benefit_ci_upper_final
    )
}

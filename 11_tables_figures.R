source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")
source("R/anchor_helpers.R")

run_logged_module("11_tables_figures", {
  reviews <- safe_read_rds(
    "derived/review_master_validated.rds",
    "01_import_validate.R"
  )

  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  )

  anchors <- safe_read_rds(
    "derived/anchor_effects.rds",
    "04_anchor_selection.R"
  ) |>
    add_conclusion_class()

  table1 <- safe_read_rds(
    "derived/Table1_review_characteristics.rds",
    "03_descriptive_reviews.R"
  )

  table5 <- safe_read_rds(
    "derived/Table5_methodological_summary.rds",
    "03_descriptive_reviews.R"
  )

  gap_table <- safe_read_rds(
    "derived/evidence_gap_table.rds",
    "06_evidence_map.R"
  )

  format_anchor_statement <- function(dat) {
    if (!nrow(dat)) {
      return("Not available")
    }

    paste(
      paste0(
        dat$outcome_label,
        ": ",
        dat$canonical_effect_text,
        " [",
        dat$publication_conclusion_class,
        "]"
      ),
      collapse = "; "
    )
  }

  context_spec <- tibble(
    Context = c(
      "Adult symptomatic MMD",
      "Hemorrhagic MMD",
      "Adult direct vs indirect",
      "Adult combined vs indirect",
      "Adult direct vs combined",
      "Adult direct/combined vs indirect",
      "Pediatric technique choice",
      "Asymptomatic/stable disease",
      "Antiplatelet therapy",
      "Anesthesia strategy",
      "Remote ischemic conditioning"
    ),
    comparison_id = c(
      "C01","C06","C02","C03","C04",
      "C15","C05","C08","C09","C10","C11"
    ),
    decision_informed = c(
      "Whether revascularization is associated with lower recurrent cerebrovascular risk than conservative management.",
      "Whether revascularization is associated with lower recurrent stroke/rebleeding risk in hemorrhagic presentation.",
      "Event-specific choice between direct and indirect bypass in adults.",
      "Event-specific choice between combined and indirect bypass in adults.",
      "Whether direct or combined bypass has a clear late ischemic advantage.",
      "Immediate-flow (direct/combined) group versus indirect bypass in adult ischemic/hemodynamic evidence.",
      "Whether angiographic superiority translates into clinical superiority in children.",
      "Whether prophylactic surgery clearly improves outcomes over surveillance.",
      "Adjunctive antiplatelet therapy, especially after bypass.",
      "Inhalational versus intravenous anesthesia during revascularization.",
      "Adjunctive remote ischemic conditioning."
    ),
    practical_interpretation = c(
      "Surgery is consistently associated with fewer future cerebrovascular events in symptomatic adults, but the evidence is observational and does not define a universal timing or technique.",
      "Revascularization is associated with lower recurrent stroke/rebleeding; mortality signals vary by review and network rankings remain fragile.",
      "Direct bypass shows lower early ischemic and late ischemic/hemorrhagic events and better functional outcomes, while perioperative hemorrhage favors indirect bypass.",
      "Combined bypass shows lower early ischemic and late ischemic/hemorrhagic events and better function, while perioperative hemorrhage favors indirect bypass.",
      "Evidence is limited; one late ischemia signal favors direct over combined and is insufficient for a universal hierarchy.",
      "Immediate-flow strategies show angiographic/recurrent-stroke signals, while hemodynamic normalization is imprecise; source definitions often pool direct and combined procedures.",
      "Direct/combined techniques improve angiographic collateralization, but perioperative TIA does not show clear superiority; surrogate success must not be equated with clinical benefit.",
      "No clear TIA advantage is demonstrated; individualized selection and hemodynamic context remain essential.",
      "Post-bypass antiplatelet therapy shows patency, TIA and functional signals, but stroke/hemorrhage effects are not uniformly demonstrated and treatment cannot substitute for revascularization.",
      "No clear difference in transient neurologic events or postoperative stroke; absence of significance is not proof of equivalence.",
      "A small-RCT meta-analysis shows a promising ischemic-stroke reduction signal; this remains an adjunctive, not replacement, strategy."
    ),
    prohibited_inference = c(
      "Do not infer a universal indication, timing window, or best surgical technique.",
      "Do not convert network ranking into definitive superiority of one bypass technique.",
      "Do not say direct bypass is globally safer; perioperative hemorrhage signal favors indirect.",
      "Do not say combined bypass is globally safer; perioperative hemorrhage signal favors indirect.",
      "Do not infer equivalence or universal superiority from one late-outcome comparison.",
      "Do not treat a pooled direct/combined group as pure direct bypass.",
      "Do not infer fewer strokes from Matsushima grade alone.",
      "Do not infer equivalence or prophylactic benefit from an imprecise single comparison.",
      "Do not claim proven stroke prevention or replacement of surgery from observational antiplatelet evidence.",
      "Do not claim anesthetic equivalence from nonsignificant pooled estimates.",
      "Do not substitute RIC for revascularization or claim durable benefit beyond the trial evidence."
    )
  )

  decision_matrix <- pmap_dfr(
    context_spec,
    function(
      Context,
      comparison_id,
      decision_informed,
      practical_interpretation,
      prohibited_inference
    ) {
      dat <- anchors |>
        filter(
          .data$comparison_id ==
            .env$comparison_id
        )

      early <- dat |>
        filter(
          outcome_code %in%
            c(
              "O01","O04",
              "O08","O09"
            )
        )

      late <- dat |>
        filter(
          outcome_code %in%
            c(
              "O02","O05","O06",
              "O07","O16","O03"
            )
        )

      surrogate <- dat |>
        filter(
          outcome_code %in%
            c(
              "O10","O11","O12"
            )
        )

      tibble(
        Context = Context,
        `Decision informed` =
          decision_informed,
        `Early safety` =
          format_anchor_statement(
            early
          ),
        `Late/clinical efficacy` =
          format_anchor_statement(
            late
          ),
        `Technical/surrogate evidence` =
          format_anchor_statement(
            surrogate
          ),
        `Anchor reviews` =
          paste(
            unique(
              dat$`Review ID`
            ),
            collapse = "; "
          ),
        `Methodological confidence` =
          paste(
            unique(
              paste0(
                "AMSTAR ",
                dat$`Master AMSTAR 2`,
                " / ROBIS ",
                dat$`Master ROBIS`
              )
            ),
            collapse = "; "
          ),
        `Practical interpretation` =
          practical_interpretation,
        `Prohibited inference` =
          prohibited_inference
      )
    }
  )

  main_effect_ids <- c(
    "E0009","E0010","E0129","E0132",
    "E0012","E0013","E0295","E0261",
    "E0053","E0105","E0063","E0064","E0110",
    "E0057","E0106","E0065","E0066","E0067",
    "E0038","E0030","E0031",
    "E0200","E0202",
    "E0264","E0093","E0094","E0243",
    "E0277","E0178","E0262","E0162",
    "E0265"
  )

  all_anchor_effects <- anchors |>
    transmute(
      `Effect ID`,
      Phenotype =
        phenotype_label,
      Comparison =
        comparison_label,
      `Canonical intervention` =
        display_intervention,
      `Canonical comparator` =
        display_comparator,
      Outcome =
        outcome_label,
      Horizon =
        horizon_label,
      Unit =
        unit_label,
      `Anchor review` =
        paste0(
          `Review ID`,
          " (",
          review_year,
          ")"
        ),
      Measure =
        measure_label,
      `Canonical effect estimate` =
        canonical_effect_text,
      `Source-reported estimate` =
        source_effect_text,
      `Orientation inverted for display` =
        display_orientation_changed,
      `Conclusion class` =
        publication_conclusion_class,
      `Precision class` =
        publication_precision_class,
      `AMSTAR 2` =
        `Master AMSTAR 2`,
      ROBIS =
        `Master ROBIS`,
      `Source adjudication` =
        final_source_adjudication_status,
      `Exact source` =
        exact_pdf_page,
      Caveat =
        validity_caveat,
      `Publication-QC note` =
        publication_qc_note
    )

  table3_main <- all_anchor_effects |>
    filter(
      `Effect ID` %in%
        main_effect_ids
    ) |>
    mutate(
      order =
        match(
          `Effect ID`,
          main_effect_ids
        )
    ) |>
    arrange(
      order
    ) |>
    select(
      -order
    )

  prognostic_registry <- read_csv(
    "config/prognostic_publication_registry.csv",
    show_col_types = FALSE
  )

  prognostic_final <- effects |>
    filter(
      analysis_role ==
        "PROGNOSTIC",
      inference_action !=
        "exclude_duplicate"
    ) |>
    left_join(
      prognostic_registry,
      by =
        "Effect ID"
    ) |>
    mutate(
      phenotype_label =
        label_code(
          phenotype_code,
          phenotype_labels
        ),
      outcome_label =
        label_code(
          outcome_code,
          outcome_labels
        ),
      measure_label =
        label_code(
          measure_code,
          measure_labels
        ),
      `Association estimate` =
        format_effect(
          measure_label,
          estimate_numeric,
          ci_lower_numeric,
          ci_upper_numeric
        )
    )

  table4_risk_factors <- prognostic_final |>
    filter(
      `Publication table role` %in%
        c(
          "MAIN",
          "MAIN_CORROBORATOR",
          "MAIN_NULL",
          "MAIN_ASSOCIATION"
        )
    ) |>
    transmute(
      `Effect ID`,
      Phenotype =
        phenotype_label,
      Predictor =
        `Display predictor`,
      `Predicted outcome` =
        outcome_label,
      `Association estimate`,
      `Final interpretation`,
      `Review ID`,
      `Publication role` =
        `Publication table role`,
      `Clinical-use guardrail` =
        "Prognostic association only; not a treatment-effect modifier or causal treatment recommendation without formal interaction evidence.",
      `Exact source` =
        exact_pdf_page,
      `Publication-QC note`
    )

  prognostic_supplement <- prognostic_final |>
    transmute(
      `Effect ID`,
      Phenotype =
        phenotype_label,
      Predictor =
        coalesce(
          `Display predictor`,
          Outcome
        ),
      `Association estimate`,
      `Publication table role`,
      `Final interpretation`,
      `Review ID`,
      `Exact source` =
        exact_pdf_page
    )

  predefined_gaps <- tibble(
    Gap = c(
      "Surgical timing after acute ischemic or hemorrhagic events",
      "Cognition, quality of life, school performance and return to work",
      "Pediatric clinical technique comparisons",
      "MMD versus etiologic moyamoya syndromes",
      "Patient-level rather than hemisphere/procedure-level outcomes",
      "Standardized early and late outcome windows",
      "External applicability beyond East Asian/high-volume centers",
      "Treatment-effect modification by hemodynamics or anatomy",
      "Reoperation, rescue surgery and late indirect-bypass failure",
      "Validation of angiographic/hemodynamic surrogates"
    ),
    `Current limitation` = c(
      "Timing inconsistently reported and no reliable universal interval.",
      "Only a very small fraction of extracted effects are patient-centered.",
      "Angiographic evidence is stronger than comparative clinical evidence.",
      "Etiologic phenotypes are often mixed or contextual only.",
      "Many effects use mixed or unclear analytical units.",
      "Early safety and late efficacy are frequently combined or not reported.",
      "Geographic and expertise concentration limits generalizability.",
      "Most reported factors are prognostic, not interaction-based modifiers.",
      "Sparse standardized reporting.",
      "Patency, Matsushima grade and perfusion are not established clinical surrogates."
    ),
    `Recommended design` = c(
      "Prospective multicenter inception cohort or pragmatic trial with prespecified event-to-surgery intervals.",
      "Core outcome set with validated neuropsychological and patient-reported measures.",
      "Prospective pediatric comparative registry with age, vessel size and MMS etiology strata.",
      "Etiology-specific cohorts and separately reported idiopathic MMD results.",
      "Patient-level hierarchical models accounting for bilateral procedures and repeated surgery.",
      "Core definitions for ≤30-day safety and >1-year efficacy.",
      "Multiregional prospective studies with center-volume and expertise adjustment.",
      "Formal treatment-by-factor interaction analyses.",
      "Long-term competing-risk and time-to-reintervention analyses.",
      "Paired technical and clinical outcomes with longitudinal validation."
    )
  )

  claim_guardrails <- tibble(
    Claim_domain = c(
      "Overall technique hierarchy",
      "Adult direct/combined vs indirect",
      "Early hemorrhagic safety",
      "Pediatric technique choice",
      "Surrogate outcomes",
      "Antiplatelet therapy",
      "Anesthesia",
      "Remote ischemic conditioning",
      "Prognostic factors",
      "Evidence certainty",
      "Timing"
    ),
    `Allowed manuscript claim` = c(
      "Preferred strategy appears phenotype-, outcome-, and horizon-dependent.",
      "Immediate-flow strategies show favorable ischemic/late-event and functional signals in adults.",
      "Indirect bypass shows a lower perioperative hemorrhage signal than direct or combined bypass in the relevant review-level comparison.",
      "Direct/combined procedures improve angiographic collateralization, while comparative clinical superiority remains unproven.",
      "Patency, Matsushima grade and perfusion should be presented as mechanistic/technical evidence.",
      "Post-bypass antiplatelet therapy has signals for patency, TIA and function, with inconsistent stroke/hemorrhage effects.",
      "No clear pooled difference in transient neurologic events or postoperative stroke.",
      "RIC has a promising ischemic-stroke signal in a small RCT evidence base.",
      "Several factors are associated with postoperative stroke risk.",
      "The review literature is predominantly AMSTAR 2 critically low and ROBIS high.",
      "Current formal evidence does not support a universal evidence-based timing window."
    ),
    `Prohibited overstatement` = c(
      "There is one universally best bypass technique.",
      "Direct or combined bypass is globally safer or definitively superior for every adult phenotype.",
      "Indirect bypass is globally superior; the signal is event-specific to perioperative hemorrhage.",
      "Better Matsushima grade proves fewer strokes or better cognition/function.",
      "Technical or hemodynamic improvement proves patient-important clinical benefit.",
      "Antiplatelet therapy replaces revascularization or definitively prevents stroke.",
      "Nonsignificance proves equivalence.",
      "RIC can replace revascularization or has proven durable benefit.",
      "A prognostic association is a validated treatment-effect modifier or individualized prediction rule.",
      "The synthesis provides high-certainty GRADE recommendations.",
      "A specific delay or early-surgery interval is universally optimal."
    )
  )

  figure_files <- dir_ls(
    "results/figures",
    recurse = TRUE,
    regexp =
      "\\.(png|pdf|svg)$",
    type = "file"
  )

  figure_index <- tibble(
    File =
      path_file(
        figure_files
      ),
    Directory =
      path_dir(
        figure_files
      ),
    Format =
      str_to_upper(
        path_ext(
          figure_files
        )
      ),
    Bytes =
      as.numeric(
        file_info(
          figure_files
        )$size
      )
  ) |>
    arrange(
      File,
      Format
    )

  manuscript_tables <- list(
    Table1_review_characteristics =
      table1,
    Table2_decision_matrix =
      decision_matrix,
    Table3_main_anchor_effects =
      table3_main,
    Table4_perioperative_risk =
      table4_risk_factors,
    Table5_methodological_summary =
      table5,
    Table6_priority_gaps =
      predefined_gaps,
    Claims_guardrail =
      claim_guardrails,
    Supplement_all_anchor_effects =
      all_anchor_effects,
    Supplement_all_prognostic =
      prognostic_supplement,
    Supplement_evidence_gaps =
      gap_table,
    Figure_index =
      figure_index
  )

  safe_write_xlsx(
    manuscript_tables,
    "results/tables/moyamoya_manuscript_tables.xlsx"
  )

  walk2(
    manuscript_tables,
    names(
      manuscript_tables
    ),
    ~ write_csv(
      .x,
      file.path(
        "results/tables",
        paste0(
          .y,
          ".csv"
        )
      ),
      na = ""
    )
  )

  operational_codes <- read_csv(
    "config/operational_codes.csv",
    show_col_types = FALSE
  )

  write_csv(
    operational_codes,
    "results/manuscript_data/analysis_data_dictionary.csv",
    na = ""
  )

  write_csv(
    anchors,
    "results/manuscript_data/anchor_effects_publication_final.csv",
    na = ""
  )

  write_csv(
    effects,
    "results/manuscript_data/effects_publication_final.csv",
    na = ""
  )

  write_csv(
    claim_guardrails,
    "results/manuscript_data/claims_guardrail.csv",
    na = ""
  )

  write_json(
    list(
      module =
        "11_tables_figures",
      main_tables =
        6,
      decision_matrix_rows =
        nrow(
          decision_matrix
        ),
      table3_main_rows =
        nrow(
          table3_main
        ),
      table4_main_rows =
        nrow(
          table4_risk_factors
        ),
      claim_guardrails =
        nrow(
          claim_guardrails
        ),
      figure_files =
        nrow(
          figure_index
        ),
      status = "PASS"
    ),
    "derived/table_figure_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Publication-final manuscript tables completed."
  )
})

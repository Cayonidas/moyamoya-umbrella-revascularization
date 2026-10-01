run_complete_pipeline <- function(clean = TRUE) {
  old_error <- getOption("error")
  on.exit(options(error = old_error), add = TRUE)
  options(error = NULL)

  cat(
    "\n========================================================\n",
    "MOYAMOYA UMBRELLA REVIEW — PUBLICATION-FINAL PIPELINE v4.0\n",
    "========================================================\n",
    sep = ""
  )

  if (clean) {
    for (dir in c("derived","results","logs")) {
      if (dir.exists(dir)) {
        unlink(
          dir,
          recursive = TRUE,
          force = TRUE
        )
      }
      dir.create(
        dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    dir.create(
      "results/tables",
      recursive = TRUE,
      showWarnings = FALSE
    )
    dir.create(
      "results/figures/main",
      recursive = TRUE,
      showWarnings = FALSE
    )
    dir.create(
      "results/figures/supplement",
      recursive = TRUE,
      showWarnings = FALSE
    )
    dir.create(
      "results/manuscript_data",
      recursive = TRUE,
      showWarnings = FALSE
    )
    dir.create(
      "results/reproducibility",
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  run_step <- function(
    file,
    expected =
      character()
  ) {
    cat(
      "\n--------------------------------------------------------\n",
      "RUNNING: ",
      file,
      "\n--------------------------------------------------------\n",
      sep = ""
    )

    source(
      file,
      local = .GlobalEnv
    )

    missing <- expected[
      !file.exists(
        expected
      )
    ]

    if (
      length(
        missing
      )
    ) {
      stop(
        "Module ",
        file,
        " did not create expected output(s): ",
        paste(
          missing,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }

  ok <- tryCatch({
    run_step(
      "00_preflight.R",
      "logs/preflight_checks.csv"
    )
    run_step(
      "01_import_validate.R",
      "derived/effects_formal_validated.rds"
    )
    run_step(
      "02_harmonize_cells.R",
      "derived/harmonized_effects.rds"
    )
    run_step(
      "02b_refine_inference.R",
      c(
        "derived/effects_refined.rds",
        "derived/forest_candidates_refined.rds"
      )
    )
    run_step(
      "02c_publication_qc.R",
      c(
        "derived/effects_publication_final.rds",
        "derived/forest_candidates_publication_final.rds",
        "results/tables/02c_publication_qc_audit.xlsx"
      )
    )
    run_step(
      "03_descriptive_reviews.R",
      "results/figures/main/Figure_1_PRISMA.pdf"
    )
    run_step(
      "04_anchor_selection.R",
      "derived/anchor_effects.rds"
    )
    run_step(
      "05_effect_displays.R",
      "results/figures/main/Figure_3_anchor_review_forests.pdf"
    )
    run_step(
      "06_evidence_map.R",
      "results/figures/main/Figure_2_phenotype_evidence_map.pdf"
    )
    run_step(
      "07_tradeoff_translation.R",
      c(
        "results/figures/main/Figure_4_early_late_tradeoff.pdf",
        "results/figures/main/Figure_5_translation_chain.pdf"
      )
    )
    run_step(
      "08_overlap.R",
      "results/tables/08_overlap_tables.xlsx"
    )
    run_step(
      "09_quality_conclusions.R",
      "results/tables/09_quality_conclusion_tables.xlsx"
    )
    run_step(
      "10_sensitivities.R",
      c(
        "results/tables/10_sensitivity_tables.xlsx",
        "results/figures/main/Figure_sensitivity_matrix.pdf"
      )
    )
    run_step(
      "11_tables_figures.R",
      "results/tables/moyamoya_manuscript_tables.xlsx"
    )
    run_step(
      "12_reproducibility.R",
      c(
        "results/reproducibility/final_pipeline_summary.json",
        "results/moyamoya_analysis_results_bundle.zip"
      )
    )

    TRUE
  }, error = function(e) {
    dir.create(
      "logs",
      recursive = TRUE,
      showWarnings = FALSE
    )

    writeLines(
      c(
        paste0(
          "Timestamp: ",
          format(
            Sys.time(),
            "%Y-%m-%d %H:%M:%S %z"
          )
        ),
        paste0(
          "R version: ",
          R.version.string
        ),
        paste0(
          "Error: ",
          conditionMessage(e)
        ),
        "",
        "Publication-final pipeline stopped safely.",
        "Do not run downstream modules manually."
      ),
      "logs/PIPELINE_FATAL_ERROR.txt",
      useBytes = TRUE
    )

    message(
      "\nPIPELINE STOPPED SAFELY\n",
      conditionMessage(e),
      "\nDiagnostic: logs/PIPELINE_FATAL_ERROR.txt"
    )

    FALSE
  })

  if (!isTRUE(ok)) {
    stop(
      "Publication-final pipeline failed safely. See logs/PIPELINE_FATAL_ERROR.txt and the last module log.",
      call. = FALSE
    )
  }

  cat(
    "\n========================================================\n",
    "PIPELINE v4.0.1 PUBLICATION-FINAL COMPLETED SUCCESSFULLY\n",
    "========================================================\n",
    "Now run:\n",
    '  source("run_tests.R")\n',
    "\nFinal bundle:\n",
    "  results/moyamoya_analysis_results_bundle.zip\n",
    "========================================================\n",
    sep = ""
  )
}

run_complete_pipeline(
  clean = TRUE
)

test_that("publication-final source adjudication is complete", {
  effects <- readRDS(
    project_file(
      "derived",
      "effects_publication_final.rds"
    )
  )
  audit <- readRDS(
    project_file(
      "derived",
      "publication_qc_registry_audit.rds"
    )
  )

  expect_equal(nrow(effects), 267)
  expect_equal(nrow(audit), 30)

  forest <- dplyr::filter(
    effects,
    forest_final_eligible
  )

  expect_equal(nrow(forest), 72)
  expect_equal(
    dplyr::n_distinct(
      forest$evidence_cell_id
    ),
    60
  )

  e0265 <- dplyr::filter(
    effects,
    `Effect ID` == "E0265"
  )
  expect_equal(
    e0265$publication_conclusion_class,
    "Borderline benefit direction (CI boundary at null)"
  )

  e0038 <- dplyr::filter(
    effects,
    `Effect ID` == "E0038"
  )
  expect_equal(
    e0038$comparison_id,
    "C15"
  )
  expect_equal(
    e0038$comparison_orientation,
    -1
  )
  expect_lt(
    e0038$display_estimate,
    1
  )

  e0105 <- dplyr::filter(
    effects,
    `Effect ID` == "E0105"
  )
  expect_gt(
    e0105$display_estimate,
    1
  )
  expect_equal(
    e0105$publication_conclusion_class,
    "Precise harm"
  )

  anesthesia <- dplyr::filter(
    effects,
    `Effect ID` %in%
      c("E0178","E0277")
  )
  expect_true(
    all(
      anesthesia$mapping_confidence ==
        "High"
    )
  )
  expect_true(
    all(
      !anesthesia$final_comparison_text_mismatch
    )
  )
})

test_that("all publication-final anchors are source-resolved", {
  anchors <- readRDS(
    project_file(
      "derived",
      "anchor_effects.rds"
    )
  )

  expect_equal(
    nrow(anchors),
    60
  )
  expect_equal(
    dplyr::n_distinct(
      anchors$evidence_cell_id
    ),
    60
  )
  expect_true(
    all(
      !anchors$final_manual_review_flag
    )
  )
  expect_true(
    all(
      !anchors$final_comparison_text_mismatch
    )
  )
  expect_true(
    all(
      anchors$comparison_orientation !=
        0
    )
  )
  expect_false(
    any(
      anchors$publication_conclusion_class ==
        "Direction not prespecified"
    )
  )

  expected <- c(
    "Precise benefit" = 39L,
    "Imprecise benefit direction" = 15L,
    "Imprecise harm direction" = 3L,
    "Precise harm" = 2L,
    "Borderline benefit direction (CI boundary at null)" = 1L
  )

  observed <- table(
    anchors$publication_conclusion_class
  )

  for (nm in names(expected)) {
    expect_equal(
      unname(
        observed[[nm]]
      ),
      expected[[nm]]
    )
  }
})

test_that("publication-final manuscript outputs exist", {
  required <- c(
    "results/figures/main/Figure_1_PRISMA.pdf",
    "results/figures/main/Figure_2_phenotype_evidence_map.pdf",
    "results/figures/main/Figure_3_anchor_review_forests.pdf",
    "results/figures/main/Figure_4_early_late_tradeoff.pdf",
    "results/figures/main/Figure_5_translation_chain.pdf",
    "results/tables/moyamoya_manuscript_tables.xlsx",
    "results/tables/Claims_guardrail.csv",
    "results/manuscript_data/claims_guardrail.csv",
    "results/reproducibility/publication_final_log_audit.csv",
    "results/moyamoya_analysis_results_bundle.zip"
  )

  expect_true(
    all(
      file.exists(
        project_file(
          required
        )
      )
    )
  )
})

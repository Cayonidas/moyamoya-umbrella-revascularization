test_that("all manuscript-critical outputs exist", {
  required <- c(
    "results/tables/moyamoya_manuscript_tables.xlsx",
    "results/figures/main/Figure_1_PRISMA.pdf",
    "results/figures/main/Figure_2_phenotype_evidence_map.pdf",
    "results/figures/main/Figure_3_anchor_review_forests.pdf",
    "results/figures/main/Figure_4_early_late_tradeoff.pdf",
    "results/figures/main/Figure_5_translation_chain.pdf",
    "results/figures/main/Figure_sensitivity_matrix.pdf",
    "results/reproducibility/file_manifest_sha256.csv",
    "results/reproducibility/final_pipeline_summary.json",
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

test_that("main analytical datasets remain internally consistent", {
  anchors <- readRDS(
    project_file(
      "derived",
      "anchor_effects.rds"
    )
  )
  sensitivities <- readRDS(
    project_file(
      "derived",
      "sensitivity_cell_comparison.rds"
    )
  )
  overlap <- readRDS(
    project_file(
      "derived",
      "cca_all_scopes.rds"
    )
  )

  expect_equal(
    nrow(anchors),
    60
  )
  expect_true(
    nrow(sensitivities) >
      0
  )
  expect_true(
    all(
      is.na(overlap$CCA) |
        overlap$CCA >=
        0
    )
  )
})

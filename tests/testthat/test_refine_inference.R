test_that("refinement produces the locked final quantitative set", {
  refined <- readRDS(project_file("derived", "effects_refined.rds"))
  forest <- readRDS(project_file("derived", "forest_candidates_refined.rds"))
  duplicates <- readRDS(project_file("derived", "duplicate_effects.rds"))

  expect_equal(nrow(refined), 267)
  expect_equal(nrow(duplicates), 12)
  expect_equal(nrow(forest), 72)
  expect_equal(dplyr::n_distinct(forest$evidence_cell_id), 60)

  expect_true("source_verification" %in% names(refined))
  expect_true("analysis_type" %in% names(refined))
  expect_true(all(nchar(refined$source_verification) > 0))

  expect_false(any(forest$comparison_id == "C00"))
  expect_false(any(forest$outcome_code == "O99"))
  expect_true(all(!forest$ratio_measure | forest$ci_lower_numeric > 0))
})

test_that("source-based inference corrections are present", {
  refined <- readRDS(project_file("derived", "effects_refined.rds"))
  get_row <- function(id) dplyr::filter(refined, `Effect ID` == id)

  expect_equal(get_row("E0012")$outcome_code, "O02")
  expect_equal(get_row("E0030")$comparison_id, "C15")
  expect_equal(get_row("E0061")$comparison_id, "C03")
  expect_equal(get_row("E0062")$comparison_id, "C02")
  expect_equal(get_row("E0227")$horizon_code, "T3")
  expect_equal(get_row("E0245")$inference_action, "exclude_duplicate")
  expect_false(get_row("E0190")$forest_final_eligible)
  expect_false(get_row("E0166")$forest_final_eligible)
})

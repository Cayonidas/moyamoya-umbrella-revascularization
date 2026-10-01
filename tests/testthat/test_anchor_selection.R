test_that("anchor selection yields one final anchor per quantitative cell", {
  anchors <- readRDS(
    project_file(
      "derived",
      "anchor_effects.rds"
    )
  )
  corroborators <- readRDS(
    project_file(
      "derived",
      "corroborator_effects.rds"
    )
  )

  expect_equal(nrow(anchors), 60)
  expect_equal(nrow(corroborators), 7)
  expect_equal(
    dplyr::n_distinct(
      anchors$evidence_cell_id
    ),
    60
  )
  expect_true(
    all(
      anchors$forest_final_eligible
    )
  )
})

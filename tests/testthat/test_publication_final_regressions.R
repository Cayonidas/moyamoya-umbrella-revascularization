test_that("main forest contains the inherited plotting aesthetics it uses", {
  dat <- readRDS(
    project_file(
      "derived",
      "main_forest_data.rds"
    )
  )

  expect_equal(nrow(dat), 18)
  expect_true("direction_group" %in% names(dat))
  expect_false(any(is.na(dat$direction_group)))
  expect_true(
    all(
      dat$direction_group %in%
        c(
          "Favors intervention",
          "Favors comparator",
          "No clear difference",
          "Not directionally interpretable"
        )
    )
  )
})

test_that("sensitivity analysis uses publication-final effect orientations", {
  effects <- readRDS(
    project_file(
      "derived",
      "effects_publication_final.rds"
    )
  )
  comparisons <- readRDS(
    project_file(
      "derived",
      "sensitivity_cell_comparison.rds"
    )
  )

  expect_true(nrow(comparisons) > 0)

  e0105 <- dplyr::filter(
    effects,
    `Effect ID` == "E0105"
  )
  expect_gt(e0105$display_estimate, 1)

  e0038 <- dplyr::filter(
    effects,
    `Effect ID` == "E0038"
  )
  expect_equal(e0038$comparison_id, "C15")
})

test_that("decision matrix is scoped to each comparison rather than all anchors", {
  table2 <- readr::read_csv(
    project_file(
      "results",
      "tables",
      "Table2_decision_matrix.csv"
    ),
    show_col_types = FALSE
  )

  expect_equal(nrow(table2), 11)

  direct_row <- dplyr::filter(
    table2,
    Context == "Adult direct vs indirect"
  )
  anesthesia_row <- dplyr::filter(
    table2,
    Context == "Anesthesia strategy"
  )

  expect_equal(nrow(direct_row), 1)
  expect_equal(nrow(anesthesia_row), 1)

  expect_false(
    stringr::str_detect(
      direct_row$`Anchor reviews`,
      "R017"
    )
  )
  expect_true(
    stringr::str_detect(
      anesthesia_row$`Anchor reviews`,
      "R017"
    )
  )
})

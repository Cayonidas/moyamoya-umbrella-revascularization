test_that("complete overlap matrix is detected structurally and reproduces the frozen audit", {
  reconstruction <- readRDS(
    project_file(
      "derived",
      "cca_reproduction_check.rds"
    )
  )
  matrix_check <- readRDS(
    project_file(
      "derived",
      "overlap_matrix_structure_check.rds"
    )
  )
  global_check <- readRDS(
    project_file(
      "derived",
      "overlap_global_count_check.rds"
    )
  )
  overlap_summary <- jsonlite::fromJSON(
    project_file(
      "derived",
      "overlap_analysis_summary.json"
    )
  )

  expect_equal(
    overlap_summary$matrix_header_row,
    4
  )
  expect_equal(
    overlap_summary$matrix_audit_boundary_row,
    489
  )
  expect_equal(
    overlap_summary$matrix_data_rows,
    482
  )
  expect_equal(
    overlap_summary$raw_author_year_keys,
    482
  )
  expect_equal(
    overlap_summary$raw_all_occurrences,
    610
  )

  expect_true(
    all(matrix_check$match)
  )
  expect_true(
    all(global_check$Match)
  )

  expect_equal(
    overlap_summary$raw_formal_unique_keys,
    472
  )
  expect_equal(
    overlap_summary$raw_formal_occurrences,
    599
  )
  expect_equal(
    overlap_summary$formal_reviews_contributing_raw_keys,
    44
  )
  expect_equal(
    overlap_summary$adjusted_formal_publications,
    479
  )
  expect_equal(
    overlap_summary$adjusted_formal_occurrences,
    588
  )

  expect_equal(
    nrow(reconstruction),
    3
  )
  expect_true(
    all(reconstruction$all_match)
  )
  expect_true(
    overlap_summary$frozen_CCA_reproduced
  )

  expect_equal(
    overlap_summary$overall_CCA_percent,
    0.5292032820313638,
    tolerance = 1e-12
  )
})

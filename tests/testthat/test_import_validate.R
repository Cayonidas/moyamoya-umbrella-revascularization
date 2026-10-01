test_that("frozen validation outputs satisfy the amendment", {
  expect_true(file.exists(project_file("derived", "effects_formal_validated.rds")))
  expect_true(file.exists(project_file("derived", "effects_context_quarantined.rds")))
  formal <- readRDS(project_file("derived", "effects_formal_validated.rds"))
  quarantined <- readRDS(project_file("derived", "effects_context_quarantined.rds"))
  expect_equal(nrow(formal), 267)
  expect_equal(nrow(quarantined), 5)
  expect_setequal(
    quarantined$`Effect ID`,
    c("E0172","E0173","E0174","E0230","E0273")
  )
  expect_true(all(formal$formal_unit == "Yes"))
  expect_true(all(!is.na(formal$`Exact PDF page`)))
})

test_that("directly forest-ready effects have valid intervals", {
  formal <- readRDS(project_file("derived", "effects_formal_validated.rds"))
  ready <- formal |> dplyr::filter(forest_ready_direct)
  expect_true(all(ready$ci_lower <= ready$estimate_numeric))
  expect_true(all(ready$estimate_numeric <= ready$ci_upper))
})

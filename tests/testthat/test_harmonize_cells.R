test_that("locked registry covers every formal effect exactly once", {
  expect_true(file.exists(project_file("derived", "harmonized_effects.rds")))
  x <- readRDS(project_file("derived", "harmonized_effects.rds"))
  expect_equal(nrow(x), 267)
  expect_equal(dplyr::n_distinct(x$`Effect ID`), 267)
  expect_false(anyNA(x$phenotype_code))
  expect_false(anyNA(x$comparison_id))
  expect_false(anyNA(x$outcome_code))
  expect_false(anyNA(x$horizon_code))
  expect_false(anyNA(x$unit_code))
  expect_false(anyNA(x$measure_code))
})

test_that("operational distributions reproduce the locked manifest", {
  x <- readRDS(project_file("derived", "harmonized_effects.rds"))
  expect_equal(sum(x$forest_candidate), 83)
  expect_equal(sum(x$comparison_id == "C00"), 110)
  expect_equal(sum(x$outcome_code == "O99"), 14)
  expect_equal(sum(x$comparison_text_mismatch), 2)
})

test_that("forest candidates satisfy all prespecified requirements", {
  x <- readRDS(project_file("derived", "harmonized_effects.rds"))
  f <- dplyr::filter(x, forest_candidate)
  expect_true(all(f$comparison_id != "C00"))
  expect_true(all(f$outcome_code != "O99"))
  expect_true(all(f$analysis_role == "COMPARATIVE"))
  expect_true(all(f$ci_parse_valid))
  expect_true(all(f$measure_code %in% c("EM_OR","EM_RR","EM_HR","EM_MD","EM_WMD")))
})

test_that("evidence cell identifiers are deterministic", {
  x <- readRDS(project_file("derived", "harmonized_effects.rds"))
  rebuilt <- paste(
    x$phenotype_code, x$comparison_id, x$outcome_code,
    x$horizon_code, x$unit_code, sep = "|"
  )
  expect_identical(x$evidence_cell_id, rebuilt)
})

test_that("noninferential codes never create a forest candidate", {
  x <- readRDS(project_file("derived", "harmonized_effects.rds"))
  expect_false(any(x$forest_candidate & x$comparison_id == "C00"))
  expect_false(any(x$forest_candidate & x$outcome_code == "O99"))
})

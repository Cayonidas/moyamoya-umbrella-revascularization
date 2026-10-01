source("00_environment.R")
library(testthat)

project_root <- normalizePath(
  getwd(), winslash = "/", mustWork = TRUE
)
Sys.setenv(MOY_PROJECT_ROOT = project_root)

message("Test project root: ", project_root)

test_dir(
  file.path(project_root, "tests", "testthat"),
  reporter = "summary"
)

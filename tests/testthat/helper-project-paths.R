# testthat may execute test files with tests/testthat as the working directory.
# Store the project root before entering test_dir() and use absolute paths.
project_root <- Sys.getenv("MOY_PROJECT_ROOT", unset = "")

if (!nzchar(project_root)) {
  candidate <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(candidate, "00_environment.R"))) {
    project_root <- candidate
  } else {
    project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  }
}

project_file <- function(...) {
  file.path(project_root, ...)
}

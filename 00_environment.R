options(stringsAsFactors = FALSE, scipen = 999)
set.seed(20260805)

required_packages <- c(
  "dplyr","tidyr","purrr","stringr","readxl","readr","tibble",
  "janitor","digest","jsonlite","writexl","cli","testthat",
  "ggplot2","scales","forcats","patchwork","ggrepel",
  "glue","fs","zip"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Missing mandatory R packages: ",
    paste(missing_packages, collapse = ", "),
    "\nRun source(\"install_packages.R\") first.",
    call. = FALSE
  )
}

dirs <- c(
  "derived","logs","results","results/tables",
  "results/figures/main","results/figures/supplement",
  "results/supplements","results/manuscript_data",
  "results/reproducibility"
)
invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

message("Environment initialized.")
message("R version: ", R.version.string)
message("Working directory: ", normalizePath(".", winslash = "/", mustWork = FALSE))

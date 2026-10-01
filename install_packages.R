# Moyamoya umbrella review — FINAL installer v3.0
# Installs ONLY required dependency classes: Depends, Imports and LinkingTo.
# It deliberately avoids the large Suggests dependency tree.

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  timeout = max(600, getOption("timeout", 60))
)

mandatory_packages <- c(
  "dplyr","tidyr","purrr","stringr","readxl","readr","tibble",
  "janitor","digest","jsonlite","writexl","cli","testthat",
  "ggplot2","scales","forcats","patchwork","ggrepel",
  "glue","fs","zip"
)
optional_packages <- c("svglite")

dependency_classes <- c("Depends","Imports","LinkingTo")
pkg_type <- if (.Platform$OS.type == "windows") "binary" else getOption("pkgType")

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

install_one <- function(pkg, optional = FALSE) {
  if (is_installed(pkg)) {
    cat(sprintf("[OK] %-16s %s\n", pkg, as.character(packageVersion(pkg))))
    return(TRUE)
  }

  cat(sprintf("[INSTALL] %s\n", pkg))
  ok <- tryCatch({
    utils::install.packages(
      pkg,
      dependencies = dependency_classes,
      type = pkg_type
    )
    is_installed(pkg)
  }, error = function(e) {
    cat(sprintf("[ERROR] %s: %s\n", pkg, conditionMessage(e)))
    FALSE
  })

  if (!ok && optional) {
    cat(sprintf(
      "[OPTIONAL] %s unavailable. SVG export will be skipped; PNG/PDF remain available.\n",
      pkg
    ))
  } else if (!ok) {
    cat(sprintf("[FAILED] %s\n", pkg))
  }
  ok
}

cat("\n========================================================\n")
cat("MOYAMOYA FINAL PIPELINE v3.0 — PACKAGE INSTALLER\n")
cat("========================================================\n")
cat("R:", R.version.string, "\n")
cat("Library:", .libPaths()[1], "\n")
cat("CRAN:", getOption("repos")[["CRAN"]], "\n\n")

invisible(vapply(mandatory_packages, install_one, logical(1), optional = FALSE))
invisible(vapply(optional_packages, install_one, logical(1), optional = TRUE))

missing <- mandatory_packages[
  !vapply(mandatory_packages, is_installed, logical(1))
]

cat("\nFINAL VERIFICATION\n")
if (length(missing)) {
  cat(paste0(" - MISSING: ", missing, collapse = "\n"), "\n")
  stop(
    "Mandatory package installation incomplete: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

cat("PASS: all mandatory packages are available.\n")
cat('Next: source("RUN_COMPLETE_PIPELINE.R")\n')

optional_missing <- optional_packages[
  !vapply(optional_packages, is_installed, logical(1))
]
if (length(optional_missing)) {
  cat(
    "Optional package(s) unavailable: ",
    paste(optional_missing, collapse = ", "),
    ". This does not block the analysis.\n",
    sep = ""
  )
}

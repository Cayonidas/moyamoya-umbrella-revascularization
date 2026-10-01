source("00_environment.R")
source("R/pipeline_helpers.R")
suppressPackageStartupMessages({
  library(digest)
  library(zip)
})

run_logged_module("12_reproducibility", {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
  }

  ensure_analysis_directories()

  session_lines <- capture.output(sessionInfo())
  writeLines(
    session_lines,
    "logs/sessionInfo.txt",
    useBytes = TRUE
  )

  package_names <- c(
    "dplyr","tidyr","purrr","stringr","readxl","readr",
    "tibble","janitor","digest","jsonlite","writexl","cli",
    "testthat","ggplot2","scales","forcats","patchwork",
    "ggrepel","glue","fs","zip"
  )
  package_versions <- tibble(
    Package = package_names,
    Version = map_chr(
      package_names,
      ~ if (requireNamespace(.x, quietly = TRUE)) {
        as.character(packageVersion(.x))
      } else {
        NA_character_
      }
    )
  )
  write_csv(
    package_versions,
    "logs/package_versions.csv",
    na = ""
  )

  input_files <- c(
    "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx",
    "data/moyamoya_umbrella_SAP_v1_analysis_specification.xlsx",
    "data/Moyamoya_Umbrella_Review_Independent_Full_Text_Extraction.xlsx",
    "config/effect_harmonization_registry.csv",
    "config/inference_refinement_registry.csv",
    "config/publication_source_adjudication_registry.csv",
    "config/prognostic_publication_registry.csv",
    "config/anchor_manual_overrides.csv"
  )
  code_files <- c(
    "00_environment.R","01_import_validate.R",
    "02_harmonize_cells.R","02b_refine_inference.R","02c_publication_qc.R",
    sprintf("%02d_%s.R", 3:12, c(
      "descriptive_reviews","anchor_selection","effect_displays",
      "evidence_map","tradeoff_translation","overlap",
      "quality_conclusions","sensitivities",
      "tables_figures","reproducibility"
    ))
  )
  code_files <- code_files[file.exists(code_files)]

  output_files <- c(
    dir_ls("derived", recurse = TRUE, type = "file"),
    dir_ls("results", recurse = TRUE, type = "file"),
    dir_ls("logs", recurse = TRUE, type = "file")
  )
  manifest_files <- unique(c(input_files, code_files, output_files))
  manifest_files <- manifest_files[file.exists(manifest_files)]

  manifest <- tibble(
    File = manifest_files,
    Role = case_when(
      str_starts(File, "data/") ~ "Input data",
      str_starts(File, "config/") ~ "Configuration",
      str_starts(File, "derived/") ~ "Derived analytical object",
      str_starts(File, "results/") ~ "Result",
      str_starts(File, "logs/") ~ "Log",
      str_detect(File, "\\.R$") ~ "Code",
      TRUE ~ "Other"
    ),
    Bytes = file_info(File)$size,
    SHA256 = map_chr(File, ~ digest(file = .x, algo = "sha256"))
  ) |>
    arrange(Role, File)

  write_csv(
    manifest,
    "results/reproducibility/file_manifest_sha256.csv",
    na = ""
  )

  summary_files <- dir_ls(
    "derived", regexp = "_summary\\.json$",
    type = "file"
  )
  module_summaries <- map(
    summary_files,
    ~ fromJSON(.x, simplifyVector = TRUE)
  )
  names(module_summaries) <- path_file(summary_files)

  required_outputs <- tibble(
    Output = c(
      "results/tables/moyamoya_manuscript_tables.xlsx",
      "results/tables/02c_publication_qc_audit.xlsx",
      "results/tables/Claims_guardrail.csv",
      "results/figures/main/Figure_1_PRISMA.pdf",
      "results/figures/main/Figure_2_phenotype_evidence_map.pdf",
      "results/figures/main/Figure_3_anchor_review_forests.pdf",
      "results/figures/main/Figure_4_early_late_tradeoff.pdf",
      "results/figures/main/Figure_5_translation_chain.pdf",
      "results/figures/main/Figure_sensitivity_matrix.pdf"
    ),
    Exists = file.exists(Output)
  )
  write_csv(
    required_outputs,
    "results/reproducibility/required_output_check.csv"
  )

  output_readme <- c(
    "# Moyamoya umbrella review analytical outputs",
    "",
    "## Main tables",
    "- `results/tables/moyamoya_manuscript_tables.xlsx`",
    "",
    "## Main figures",
    "- Figure 1: PRISMA-style study-selection flow",
    "- Figure 2: phenotype-informed evidence map",
    "- Figure 3: anchor-review forest panels without umbrella-level pooling",
    "- Figure 4: event-specific early/late surgical trade-off matrix",
    "- Figure 5: technical-to-patient-important translation ladder",
    "- Sensitivity matrix",
    "",
    "## Reproducibility",
    "- SHA-256 manifest",
    "- R session information",
    "- package versions",
    "- module logs",
    "- complete publication-final effect and anchor datasets",
    "- source-adjudication and manuscript-claims guardrails",
    "",
    "The frozen source workbook is never overwritten."
  )
  writeLines(
    output_readme,
    "results/README_outputs.md",
    useBytes = TRUE
  )


  # Manuscript-writing key numbers.
  key_objects <- list(
    refinement = if (file.exists(
      "derived/inference_refinement_summary.json"
    )) fromJSON(
      "derived/inference_refinement_summary.json"
    ) else list(),
    publication_qc = if (file.exists(
      "derived/publication_qc_summary.json"
    )) fromJSON(
      "derived/publication_qc_summary.json"
    ) else list(),
    anchors = if (file.exists(
      "derived/anchor_selection_summary.json"
    )) fromJSON(
      "derived/anchor_selection_summary.json"
    ) else list(),
    descriptive = if (file.exists(
      "derived/descriptive_reviews_summary.json"
    )) fromJSON(
      "derived/descriptive_reviews_summary.json"
    ) else list(),
    overlap = if (file.exists(
      "derived/overlap_analysis_summary.json"
    )) fromJSON(
      "derived/overlap_analysis_summary.json"
    ) else list(),
    sensitivity = if (file.exists(
      "derived/sensitivity_summary.json"
    )) fromJSON(
      "derived/sensitivity_summary.json"
    ) else list()
  )

  write_json(
    key_objects,
    "results/manuscript_data/key_results_for_writing.json",
    pretty = TRUE, auto_unbox = TRUE
  )

  summary_lines <- c(
    "# Moyamoya umbrella review — analytical run summary",
    "",
    paste0("- Formal reviews: ",
           key_objects$descriptive$formal_reviews %||% "NR"),
    paste0("- Tier 1 reviews: ",
           key_objects$descriptive$tier1 %||% "NR"),
    paste0("- Tier 2 reviews: ",
           key_objects$descriptive$tier2 %||% "NR"),
    paste0("- Refined formal effects: ",
           key_objects$refinement$formal_source_rows %||% "NR"),
    paste0("- Forest-eligible effects: ",
           key_objects$refinement$final_forest_candidates %||% "NR"),
    paste0("- Quantitative evidence cells: ",
           key_objects$refinement$final_forest_cells %||% "NR"),
    paste0("- Review anchors: ",
           key_objects$anchors$anchors %||% "NR"),
    paste0("- Corroborators: ",
           key_objects$anchors$corroborators %||% "NR"),
    paste0("- High-priority cohort families: ",
           key_objects$overlap$cohort_families %||% "NR"),
    paste0("- Sensitivity direction reversals: ",
           key_objects$sensitivity$direction_reversals %||% "NR")
  )
  writeLines(
    summary_lines,
    "results/manuscript_data/analytical_run_summary.md",
    useBytes = TRUE
  )



  # Publication-final computational log audit.
  log_files <- dir_ls(
    "logs",
    regexp = "\\.log$",
    type = "file"
  )

  log_text <- map_chr(
    log_files,
    ~ paste(
      readLines(
        .x,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )
  )

  warning_audit <- tibble(
    Log = path_file(log_files),
    Contains_NaN_warning = str_detect(
      log_text,
      regex(
        "NaN|NaNs produzidos",
        ignore_case = TRUE
      )
    ),
    Contains_FATAL = str_detect(
      log_text,
      "FATAL ERROR|Status: FAIL"
    )
  )

  write_csv(
    warning_audit,
    "results/reproducibility/publication_final_log_audit.csv"
  )

  if (
    any(
      warning_audit$Contains_NaN_warning
    ) ||
    any(
      warning_audit$Contains_FATAL
    )
  ) {
    stop(
      "Publication-final log audit detected NaN/fatal patterns. See results/reproducibility/publication_final_log_audit.csv",
      call. = FALSE
    )
  }

  final_summary <- list(
    pipeline_version = "4.0.1-publication-final",
    run_completed = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    R_version = R.version.string,
    required_outputs_present = all(required_outputs$Exists),
    required_output_check = required_outputs,
    module_summaries = module_summaries
  )
  write_json(
    final_summary,
    "results/reproducibility/final_pipeline_summary.json",
    pretty = TRUE, auto_unbox = TRUE
  )

  # Build a user-facing result bundle after all files have been written.
  bundle_path <- "results/moyamoya_analysis_results_bundle.zip"
  if (file.exists(bundle_path)) file.remove(bundle_path)
  files_to_zip <- dir_ls(
    "results", recurse = TRUE, type = "file"
  )
  files_to_zip <- files_to_zip[
    normalizePath(files_to_zip, mustWork = FALSE) !=
      normalizePath(bundle_path, mustWork = FALSE)
  ]
  zipr(bundle_path, files = files_to_zip, root = ".")

  if (!all(required_outputs$Exists)) {
    missing <- required_outputs$Output[!required_outputs$Exists]
    cli_warn(
      "Some prespecified outputs were not created: {paste(missing, collapse=', ')}"
    )
  }

  message("Reproducibility module completed.")
  message("Result bundle: ", bundle_path)
})

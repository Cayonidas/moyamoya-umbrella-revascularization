source("00_environment.R")
source("R/pipeline_helpers.R")

run_logged_module("00_preflight", {
  required_files <- c(
    "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx",
    "data/moyamoya_umbrella_SAP_v1_analysis_specification.xlsx",
    "data/Moyamoya_Umbrella_Review_Independent_Full_Text_Extraction.xlsx",
    "config/validation_config.json",
    "config/harmonization_config.json",
    "config/effect_harmonization_registry.csv",
    "config/inference_refinement_registry.csv",
    "config/publication_source_adjudication_registry.csv",
    "config/prognostic_publication_registry.csv",
    "config/operational_codes.csv",
    "config/study_flow.json"
  )

  missing <- required_files[!file.exists(required_files)]
  if (length(missing)) {
    stop(
      "Missing required project files: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  master_sheets <- excel_sheets(
    "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx"
  )

  required_sheets <- c(
    "Review master","Outcome effects final",
    "AMSTAR 2 items","ROBIS items",
    "Primary publications","Publication occurrences",
    "Cohort families","CCA adjusted","PRISMA disposition"
  )

  missing_sheets <- setdiff(
    required_sheets, master_sheets
  )
  if (length(missing_sheets)) {
    stop(
      "Frozen master is missing sheets: ",
      paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  flow <- fromJSON(
    "config/study_flow.json",
    simplifyVector = TRUE
  )
  if (
    flow$reports_assessed_full_text !=
      flow$formal_umbrella_units +
      flow$context_only_reports
  ) {
    stop(
      "Study-flow counts are internally inconsistent.",
      call. = FALSE
    )
  }

  preflight <- tibble(
    Check = c(
      "Required project files",
      "Required frozen-master sheets",
      "Study-flow accounting",
      "Mandatory R packages"
    ),
    Status = "PASS",
    Detail = c(
      length(required_files),
      length(required_sheets),
      flow$reports_assessed_full_text,
      length(required_packages)
    )
  )

  write_csv(
    preflight,
    "logs/preflight_checks.csv"
  )
})

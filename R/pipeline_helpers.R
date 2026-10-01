suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(readxl)
  library(tibble)
  library(jsonlite)
  library(writexl)
  library(ggplot2)
  library(scales)
  library(forcats)
  library(glue)
  library(cli)
  library(fs)
})

ensure_analysis_directories <- function() {
  dirs <- c(
    "derived","logs","results","results/tables",
    "results/figures/main","results/figures/supplement",
    "results/supplements","results/manuscript_data",
    "results/reproducibility"
  )
  walk(dirs, ~ dir_create(.x, recurse = TRUE))
  invisible(dirs)
}

append_log <- function(path, text) {
  cat(
    paste0(text, collapse = "\n"), "\n",
    file = path, append = TRUE, sep = ""
  )
}

run_logged_module <- function(module, code) {
  ensure_analysis_directories()
  log_path <- file.path("logs", paste0(module, ".log"))
  if (file.exists(log_path)) file.remove(log_path)

  start <- Sys.time()
  header <- c(
    "========================================================",
    paste0("MODULE: ", module),
    paste0("START: ", format(start, "%Y-%m-%d %H:%M:%S %z")),
    paste0(
      "WD: ",
      normalizePath(".", winslash = "/", mustWork = FALSE)
    ),
    "========================================================"
  )
  append_log(log_path, header)
  message("\n>>> ", module)

  warnings_seen <- character()

  value <- tryCatch(
    withCallingHandlers(
      eval.parent(substitute(code)),
      warning = function(w) {
        msg <- conditionMessage(w)
        warnings_seen <<- c(warnings_seen, msg)
        append_log(log_path, paste0("[WARNING] ", msg))
      }
    ),
    error = function(e) {
      error_block <- c(
        "",
        "================ FATAL ERROR ================",
        paste0("Message: ", conditionMessage(e)),
        paste0("Class: ", paste(class(e), collapse = ", ")),
        "",
        "Calls:",
        capture.output(sys.calls()),
        "============================================="
      )
      append_log(log_path, error_block)
      writeLines(
        error_block,
        file.path("logs", paste0(module, "_ERROR.txt")),
        useBytes = TRUE
      )
      message("FAILED: ", module)
      message("Diagnostic: logs/", module, "_ERROR.txt")
      stop(conditionMessage(e), call. = FALSE)
    }
  )

  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  footer <- c(
    "",
    "================ COMPLETED ===================",
    paste0("Seconds: ", sprintf("%.2f", elapsed)),
    paste0("Warnings: ", length(warnings_seen)),
    "Status: PASS",
    "============================================="
  )
  append_log(log_path, footer)
  message("PASS: ", module, " [", sprintf("%.1f", elapsed), " s]")
  invisible(value)
}

require_columns <- function(data, columns, object_name = deparse(substitute(data))) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      object_name, " is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

safe_read_rds <- function(path, module_hint = NULL) {
  if (!file.exists(path)) {
    hint <- if (is.null(module_hint)) "" else paste0(
      " Run ", module_hint, " first."
    )
    stop("Required file not found: ", path, ".", hint, call. = FALSE)
  }
  readRDS(path)
}

write_csv_and_rds <- function(data, path_without_extension) {
  dir_create(path_dir(path_without_extension), recurse = TRUE)
  write_csv(data, paste0(path_without_extension, ".csv"), na = "")
  saveRDS(data, paste0(path_without_extension, ".rds"))
  invisible(data)
}

safe_write_xlsx <- function(objects, path) {
  # Excel worksheet names must be <=31 chars and unique.
  nm <- names(objects)
  short <- substr(nm, 1, 31)
  if (anyDuplicated(short)) {
    short <- make.unique(short, sep = "_")
    short <- substr(short, 1, 31)
  }
  names(objects) <- short
  write_xlsx(objects, path)
}

save_plot_multiformat <- function(plot, base_path, width, height, dpi = 450) {
  dir_create(path_dir(base_path), recurse = TRUE)
  ggsave(
    paste0(base_path, ".png"), plot,
    width = width, height = height, dpi = dpi, bg = "white"
  )
  ggsave(
    paste0(base_path, ".pdf"), plot,
    width = width, height = height,
    device = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf,
    bg = "white"
  )
  if (requireNamespace("svglite", quietly = TRUE)) {
    ggsave(
      paste0(base_path, ".svg"), plot,
      width = width, height = height,
      device = svglite::svglite, bg = "white"
    )
  }
  invisible(base_path)
}

theme_moyamoya <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", colour = "#17365D"),
      plot.subtitle = element_text(colour = "#404040"),
      strip.text = element_text(face = "bold", colour = "#17365D"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      plot.caption = element_text(
        size = base_size * 0.8, colour = "#666666"
      )
    )
}

yes_like <- function(x) {
  x <- str_to_lower(coalesce(as.character(x), ""))
  str_detect(x, "^yes|yes/partial|reported|available") &
    !str_detect(x, "^no|not reported|no/nr")
}

quality_rank_amstar <- function(x) {
  recode(
    as.character(x),
    "High" = 4L, "Moderate" = 3L,
    "Low" = 2L, "Critically low" = 1L,
    .default = 0L
  )
}

quality_rank_robis <- function(x) {
  recode(
    as.character(x),
    "Low" = 3L, "Unclear" = 2L,
    "High" = 1L,
    .default = 0L
  )
}

parse_last_year_vec <- function(text, publication_year) {
  text <- coalesce(as.character(text), "")
  publication_year <- suppressWarnings(as.numeric(publication_year))

  vapply(seq_along(text), function(i) {
    years <- str_extract_all(text[[i]], "(?:19|20)\\d{2}")[[1]]
    years <- suppressWarnings(as.numeric(years))
    years <- years[is.finite(years)]
    pub <- publication_year[[i]]

    if (!length(years)) {
      if (is.finite(pub)) return(pub - 1)
      return(NA_real_)
    }

    candidate <- max(years)
    if (is.finite(pub) && candidate < pub - 10) {
      candidate <- pub - 1
    }
    candidate
  }, numeric(1))
}

parse_study_count_vec <- function(x) {
  x <- coalesce(as.character(x), "")

  vapply(x, function(text) {
    patterns <- c(
      "(?i)studies?\\s*:\\s*(\\d+)",
      "(?i)(\\d+)\\s+(?:studies|articles|reports)"
    )
    for (pattern in patterns) {
      m <- str_match(text, pattern)
      if (nrow(m) && !is.na(m[1,2])) {
        return(as.numeric(m[1,2]))
      }
    }
    nums <- suppressWarnings(
      as.numeric(str_extract_all(text, "\\b\\d+\\b")[[1]])
    )
    nums <- nums[is.finite(nums)]
    if (!length(nums)) return(NA_real_)
    nums[[1]]
  }, numeric(1))
}

format_effect_one <- function(measure, estimate, lower, upper, digits = 2) {
  if (is.na(estimate)) return("NR")
  if (is.na(lower) || is.na(upper)) {
    return(sprintf("%s %.*f", measure, digits, estimate))
  }
  sprintf(
    "%s %.*f (%.*f to %.*f)",
    measure, digits, estimate,
    digits, lower, digits, upper
  )
}

format_effect <- function(measure, estimate, lower, upper, digits = 2) {
  n <- max(length(measure), length(estimate), length(lower), length(upper))
  measure <- rep(measure, length.out = n)
  estimate <- rep(estimate, length.out = n)
  lower <- rep(lower, length.out = n)
  upper <- rep(upper, length.out = n)

  vapply(seq_len(n), function(i) {
    format_effect_one(
      measure[[i]], estimate[[i]],
      lower[[i]], upper[[i]], digits
    )
  }, character(1))
}

first_nonempty <- function(x, default = "") {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  if (length(x)) x[[1]] else default
}

effect_stage <- function(outcome_code) {
  case_when(
    outcome_code == "O11" ~ "Technical",
    outcome_code == "O10" ~ "Angiographic",
    outcome_code == "O12" ~ "Hemodynamic",
    outcome_code %in% c(
      "O01","O02","O03","O04","O05",
      "O07","O08","O09","O15","O16"
    ) ~ "Clinical events/safety",
    outcome_code == "O06" ~ "Functional",
    outcome_code %in% c("O13","O14") ~ "Patient-centered",
    TRUE ~ "Other/composite"
  )
}

assert_file <- function(path, label = path) {
  if (!file.exists(path)) {
    stop("Expected output not created: ", label, " [", path, "]", call. = FALSE)
  }
  invisible(TRUE)
}


safe_log_positive <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x) & x > 0
  out[ok] <- log(x[ok])
  out
}

canonicalize_effect_display <- function(data) {
  data |>
    mutate(
      source_estimate_numeric = estimate_numeric,
      source_ci_lower_numeric = ci_lower_numeric,
      source_ci_upper_numeric = ci_upper_numeric,
      display_intervention = case_when(
        comparison_orientation == -1 ~ comparator_analysis,
        TRUE ~ intervention_analysis
      ),
      display_comparator = case_when(
        comparison_orientation == -1 ~ intervention_analysis,
        TRUE ~ comparator_analysis
      ),
      display_estimate = case_when(
        ratio_measure & comparison_orientation == 1 ~ estimate_numeric,
        ratio_measure & comparison_orientation == -1 &
          !is.na(estimate_numeric) & estimate_numeric > 0 ~ 1 / estimate_numeric,
        continuous_measure & comparison_orientation == 1 ~ estimate_numeric,
        continuous_measure & comparison_orientation == -1 ~ -estimate_numeric,
        TRUE ~ estimate_numeric
      ),
      display_ci_lower = case_when(
        ratio_measure & comparison_orientation == 1 ~ ci_lower_numeric,
        ratio_measure & comparison_orientation == -1 &
          !is.na(ci_upper_numeric) & ci_upper_numeric > 0 ~ 1 / ci_upper_numeric,
        continuous_measure & comparison_orientation == 1 ~ ci_lower_numeric,
        continuous_measure & comparison_orientation == -1 ~ -ci_upper_numeric,
        TRUE ~ ci_lower_numeric
      ),
      display_ci_upper = case_when(
        ratio_measure & comparison_orientation == 1 ~ ci_upper_numeric,
        ratio_measure & comparison_orientation == -1 &
          !is.na(ci_lower_numeric) & ci_lower_numeric > 0 ~ 1 / ci_lower_numeric,
        continuous_measure & comparison_orientation == 1 ~ ci_upper_numeric,
        continuous_measure & comparison_orientation == -1 ~ -ci_lower_numeric,
        TRUE ~ ci_upper_numeric
      ),
      canonical_effect_scale = case_when(
        ratio_measure ~ safe_log_positive(display_estimate),
        continuous_measure ~ display_estimate,
        TRUE ~ NA_real_
      ),
      canonical_ci_a = case_when(
        ratio_measure ~ safe_log_positive(display_ci_lower),
        continuous_measure ~ display_ci_lower,
        TRUE ~ NA_real_
      ),
      canonical_ci_b = case_when(
        ratio_measure ~ safe_log_positive(display_ci_upper),
        continuous_measure ~ display_ci_upper,
        TRUE ~ NA_real_
      ),
      benefit_axis_effect = if_else(
        outcome_benefit_multiplier != 0,
        canonical_effect_scale * outcome_benefit_multiplier,
        NA_real_
      ),
      benefit_ci_a = if_else(
        outcome_benefit_multiplier != 0,
        canonical_ci_a * outcome_benefit_multiplier,
        NA_real_
      ),
      benefit_ci_b = if_else(
        outcome_benefit_multiplier != 0,
        canonical_ci_b * outcome_benefit_multiplier,
        NA_real_
      ),
      benefit_ci_lower_final = pmin(benefit_ci_a, benefit_ci_b, na.rm = TRUE),
      benefit_ci_upper_final = pmax(benefit_ci_a, benefit_ci_b, na.rm = TRUE),
      benefit_ci_lower_final = if_else(
        is.infinite(benefit_ci_lower_final), NA_real_, benefit_ci_lower_final
      ),
      benefit_ci_upper_final = if_else(
        is.infinite(benefit_ci_upper_final), NA_real_, benefit_ci_upper_final
      ),
      publication_conclusion_class = case_when(
        is.na(benefit_axis_effect) ~ "Direction not prespecified",
        continuous_measure &
          !is.na(benefit_ci_lower_final) &
          abs(benefit_ci_lower_final) <= 1e-12 &
          benefit_axis_effect > 0 ~
            "Borderline benefit direction (CI boundary at null)",
        !is.na(benefit_ci_lower_final) & benefit_ci_lower_final > 0 ~
          "Precise benefit",
        !is.na(benefit_ci_upper_final) & benefit_ci_upper_final < 0 ~
          "Precise harm",
        benefit_axis_effect > 0 ~ "Imprecise benefit direction",
        benefit_axis_effect < 0 ~ "Imprecise harm direction",
        TRUE ~ "Compatible with no difference"
      ),
      publication_precision_class = case_when(
        publication_conclusion_class %in% c("Precise benefit","Precise harm") ~
          "CI excludes null",
        publication_conclusion_class ==
          "Borderline benefit direction (CI boundary at null)" ~
          "CI boundary rounds to null",
        str_detect(publication_conclusion_class, "Imprecise|Compatible") ~
          "CI includes null",
        TRUE ~ "Not orientable"
      ),
      canonical_effect_text = format_effect(
        measure_label, display_estimate, display_ci_lower, display_ci_upper
      ),
      source_effect_text = format_effect(
        measure_label,
        source_estimate_numeric,
        source_ci_lower_numeric,
        source_ci_upper_numeric
      ),
      display_orientation_changed = comparison_orientation == -1
    )
}

conclusion_direction <- function(x) {
  case_when(
    str_detect(x, "Precise benefit|Imprecise benefit|Borderline benefit") ~
      "Favors intervention",
    str_detect(x, "Precise harm|Imprecise harm") ~
      "Favors comparator",
    str_detect(x, "Compatible") ~ "No clear difference",
    TRUE ~ "Not directionally interpretable"
  )
}

source("00_environment.R")
source("R/pipeline_helpers.R")
source("R/labels.R")

run_logged_module("08_overlap", {
  master_path <- "data/moyamoya_umbrella_master_extraction_FROZEN_v2.xlsx"
  full_overlap_path <- "data/Moyamoya_Umbrella_Review_Independent_Full_Text_Extraction.xlsx"

  reviews <- safe_read_rds(
    "derived/review_master_validated.rds",
    "01_import_validate.R"
  )
  effects <- safe_read_rds(
    "derived/effects_publication_final.rds",
    "02c_publication_qc.R"
  )

  require_columns(
    reviews,
    c(
      "Review ID","Formal umbrella unit",
      "Master tier","Evidence domain",
      "Year","Editorial family/update"
    ),
    "review_master_validated"
  )

  if (!file.exists(full_overlap_path)) {
    stop(
      "Full primary-study overlap workbook is missing: ",
      full_overlap_path,
      call. = FALSE
    )
  }

  repeated_occurrences <- read_excel(
    master_path,
    sheet = "Publication occurrences"
  )

  adjudication <- read_excel(
    master_path,
    sheet = "Overlap adjudication"
  )

  cohort_families <- read_excel(
    master_path,
    sheet = "Cohort families"
  )

  cca_frozen <- read_excel(
    master_path,
    sheet = "CCA adjusted"
  )

  # ==================================================================
  # 1. READ THE COMPLETE PRIMARY-STUDY MATRIX WITHOUT HARD-CODED
  #    skip/n_max OFFSETS.
  #
  # The source sheet has title/description/blank rows before the matrix
  # and a long-form audit trail after it. We detect both boundaries by
  # their literal labels, so future row insertions cannot shift the
  # extraction window.
  # ==================================================================
  raw_sheet <- read_excel(
    full_overlap_path,
    sheet = "Primary-study overlap",
    col_names = FALSE,
    .name_repair = "minimal"
  )

  first_col <- as.character(raw_sheet[[1]])

  header_candidates <- which(
    first_col == "Primary first author"
  )
  audit_candidates <- which(
    first_col == "Long-form primary-study extraction"
  )

  if (length(header_candidates) != 1L) {
    stop(
      "Expected exactly one 'Primary first author' matrix header row; found ",
      length(header_candidates),
      ".",
      call. = FALSE
    )
  }

  if (length(audit_candidates) != 1L) {
    stop(
      "Expected exactly one 'Long-form primary-study extraction' boundary row; found ",
      length(audit_candidates),
      ".",
      call. = FALSE
    )
  }

  header_row <- header_candidates[[1]]
  audit_row <- audit_candidates[[1]]

  if (audit_row <= header_row + 1L) {
    stop(
      "Primary-study overlap matrix boundaries are invalid.",
      call. = FALSE
    )
  }

  header_values <- as.character(
    unlist(
      raw_sheet[
        header_row,
        ,
        drop = TRUE
      ],
      use.names = FALSE
    )
  )

  data_block <- raw_sheet[
    (header_row + 1L):(audit_row - 1L),
    ,
    drop = FALSE
  ]

  find_header <- function(label) {
    idx <- which(header_values == label)
    if (length(idx) != 1L) {
      stop(
        "Expected exactly one matrix column named '",
        label,
        "'; found ",
        length(idx),
        ".",
        call. = FALSE
      )
    }
    idx[[1]]
  }

  author_idx <- find_header(
    "Primary first author"
  )
  year_idx <- find_header("Year")
  key_idx <- find_header(
    "Normalized author-year key"
  )
  declared_count_idx <- find_header(
    "Number of reviews containing key"
  )

  review_idx <- which(
    str_detect(
      header_values,
      "^R\\d{3}$"
    )
  )

  if (!length(review_idx)) {
    stop(
      "No R### review-membership columns were detected in the primary-study matrix.",
      call. = FALSE
    )
  }

  review_columns <- header_values[
    review_idx
  ]

  # Build a clean matrix using only the explicit metadata and R###
  # membership columns. Blank trailing columns cannot affect parsing.
  raw_matrix <- tibble(
    `Primary first author` =
      as.character(data_block[[author_idx]]),
    Year =
      suppressWarnings(
        as.numeric(data_block[[year_idx]])
      ),
    `Normalized author-year key` =
      as.character(data_block[[key_idx]]),
    `Number of reviews containing key` =
      suppressWarnings(
        as.numeric(
          data_block[[declared_count_idx]]
        )
      )
  )

  for (j in seq_along(review_idx)) {
    raw_matrix[[
      review_columns[[j]]
    ]] <- as.character(
      data_block[[review_idx[[j]]]]
    )
  }

  # Remove only genuine blank spacer rows between matrix and audit trail.
  raw_matrix <- raw_matrix |>
    filter(
      !is.na(
        `Normalized author-year key`
      ),
      str_trim(
        `Normalized author-year key`
      ) != ""
    )

  raw_key_count <- n_distinct(
    raw_matrix$`Normalized author-year key`
  )

  # Structural checks from the source workbook itself.
  if (nrow(raw_matrix) != 482L) {
    write_csv(
      raw_matrix,
      "derived/OVERLAP_MATRIX_ROWCOUNT_DIAGNOSTIC.csv"
    )
    stop(
      "The detected primary-study matrix should contain 482 data rows; found ",
      nrow(raw_matrix),
      ". See derived/OVERLAP_MATRIX_ROWCOUNT_DIAGNOSTIC.csv",
      call. = FALSE
    )
  }

  if (raw_key_count != 482L) {
    duplicate_keys <- raw_matrix |>
      count(
        `Normalized author-year key`,
        name = "Rows"
      ) |>
      filter(Rows > 1L)

    write_csv(
      duplicate_keys,
      "derived/OVERLAP_DUPLICATE_KEY_DIAGNOSTIC.csv"
    )

    stop(
      "The 482 matrix rows should contain 482 unique normalized author-year keys; found ",
      raw_key_count,
      ". See derived/OVERLAP_DUPLICATE_KEY_DIAGNOSTIC.csv",
      call. = FALSE
    )
  }

  # Confirm that every row's declared review count equals the actual
  # number of X marks in its R### columns.
  membership_matrix <- as.data.frame(
    raw_matrix[
      review_columns
    ],
    stringsAsFactors = FALSE
  )

  actual_membership_count <- rowSums(
    membership_matrix == "X",
    na.rm = TRUE
  )

  membership_check <- raw_matrix |>
    transmute(
      `Primary first author`,
      Year,
      `Normalized author-year key`,
      declared =
        `Number of reviews containing key`,
      actual =
        actual_membership_count,
      match =
        declared == actual
    )

  if (
    any(
      is.na(membership_check$match) |
      !membership_check$match
    )
  ) {
    bad_membership <- membership_check |>
      filter(
        is.na(match) |
        !match
      )

    write_csv(
      bad_membership,
      "derived/OVERLAP_MEMBERSHIP_COUNT_FAILURE.csv"
    )

    stop(
      "One or more overlap-matrix rows have an X-count that does not match the declared review count. ",
      "See derived/OVERLAP_MEMBERSHIP_COUNT_FAILURE.csv",
      call. = FALSE
    )
  }

  raw_occurrences <- raw_matrix |>
    select(
      `Normalized author-year key`,
      all_of(review_columns)
    ) |>
    pivot_longer(
      cols = all_of(review_columns),
      names_to = "Review ID",
      values_to = "Occurrence"
    ) |>
    filter(Occurrence == "X") |>
    transmute(
      author_year_key =
        `Normalized author-year key`,
      `Review ID`
    ) |>
    distinct()

  # ==================================================================
  # 2. REPLACE REPEATED AUTHOR-YEAR KEYS WITH THE FROZEN
  #    PUBLICATION-LEVEL ADJUDICATION.
  # ==================================================================
  adjudicated_keys <- adjudication |>
    transmute(
      author_year_key =
        `Original author-year key`
    ) |>
    filter(
      !is.na(author_year_key),
      author_year_key != ""
    ) |>
    distinct()

  repeated_map <- repeated_occurrences |>
    filter(
      `Primary publication ID` != "REMOVED"
    ) |>
    transmute(
      author_year_key =
        `Primary-study key`,
      `Review ID`,
      publication_id =
        `Primary publication ID`,
      identity_source =
        "Adjudicated repeated author-year key"
    ) |>
    distinct()

  singleton_occurrences <- raw_occurrences |>
    anti_join(
      adjudicated_keys,
      by = "author_year_key"
    ) |>
    mutate(
      publication_id =
        paste0(
          "SINGLETON::",
          author_year_key
        ),
      identity_source =
        "Singleton author-year publication proxy"
    )

  adjudicated_repeated_occurrences <-
    raw_occurrences |>
    semi_join(
      adjudicated_keys,
      by = "author_year_key"
    ) |>
    inner_join(
      repeated_map,
      by = c(
        "author_year_key",
        "Review ID"
      )
    )

  removed_occurrences <- raw_occurrences |>
    semi_join(
      adjudicated_keys,
      by = "author_year_key"
    ) |>
    anti_join(
      repeated_map,
      by = c(
        "author_year_key",
        "Review ID"
      )
    ) |>
    mutate(
      reason =
        "Removed artifact or adjudication produced no retained publication occurrence"
    )

  adjusted_occurrences <- bind_rows(
    singleton_occurrences,
    adjudicated_repeated_occurrences
  ) |>
    select(
      publication_id,
      author_year_key,
      `Review ID`,
      identity_source
    ) |>
    distinct(
      publication_id,
      `Review ID`,
      .keep_all = TRUE
    )

  # ==================================================================
  # 3. FORMAL-REVIEW METADATA. New names prevent join collisions.
  # ==================================================================
  formal_review_meta <- reviews |>
    filter(
      `Formal umbrella unit` == "Yes"
    ) |>
    transmute(
      `Review ID`,
      review_tier =
        `Master tier`,
      evidence_domain =
        `Evidence domain`,
      review_year =
        suppressWarnings(
          as.numeric(Year)
        )
    )

  raw_formal <- raw_occurrences |>
    inner_join(
      formal_review_meta,
      by = "Review ID"
    )

  adjusted_formal <- adjusted_occurrences |>
    inner_join(
      formal_review_meta,
      by = "Review ID"
    )

  # ==================================================================
  # 4. VALIDATE THE RECONSTRUCTION AGAINST THE FROZEN GLOBAL COUNTS
  #    BEFORE CALCULATING ANY NEW OVERLAP STATISTIC.
  # ==================================================================
  frozen_global <- cca_frozen |>
    filter(
      Scope ==
        "Tier 1 + Tier 2 formal evidence base"
    )

  if (nrow(frozen_global) != 1L) {
    stop(
      "Frozen CCA table does not contain exactly one overall formal-evidence row.",
      call. = FALSE
    )
  }

  raw_formal_keys <- n_distinct(
    raw_formal$author_year_key
  )
  raw_formal_occurrences <- nrow(
    distinct(
      raw_formal,
      author_year_key,
      `Review ID`
    )
  )
  raw_formal_reviews <- n_distinct(
    raw_formal$`Review ID`
  )
  adjusted_formal_publications <-
    n_distinct(
      adjusted_formal$publication_id
    )
  adjusted_formal_occurrences <-
    nrow(
      distinct(
        adjusted_formal,
        publication_id,
        `Review ID`
      )
    )

  global_count_check <- tibble(
    Metric = c(
      "Raw unique author-year keys",
      "Raw total occurrences",
      "Reviews contributing publications",
      "Adjusted unique primary publications",
      "Adjusted total occurrences"
    ),
    Observed = c(
      raw_formal_keys,
      raw_formal_occurrences,
      raw_formal_reviews,
      adjusted_formal_publications,
      adjusted_formal_occurrences
    ),
    Frozen = c(
      as.numeric(
        frozen_global$`Raw unique author-year keys`
      ),
      as.numeric(
        frozen_global$`Raw total occurrences`
      ),
      as.numeric(
        frozen_global$`Reviews contributing publications`
      ),
      as.numeric(
        frozen_global$`Adjusted unique primary publications`
      ),
      as.numeric(
        frozen_global$`Adjusted total occurrences`
      )
    )
  ) |>
    mutate(Match = Observed == Frozen)

  if (!all(global_count_check$Match)) {
    write_csv(
      global_count_check,
      "derived/OVERLAP_GLOBAL_COUNT_FAILURE.csv"
    )
    stop(
      "The complete overlap reconstruction does not reproduce the frozen global publication counts. ",
      "See derived/OVERLAP_GLOBAL_COUNT_FAILURE.csv",
      call. = FALSE
    )
  }

  # ==================================================================
  # 5. CORRECTED COVERED AREA.
  #
  # c = reviews contributing raw publication keys in the review set.
  # r = adjusted unique publication identities.
  # N = adjusted publication-review occurrences.
  #
  # This preserves c for reviews whose only raw tokens were later
  # adjudicated as non-publication artifacts, matching the frozen audit.
  # ==================================================================
  calc_adjusted_cca_for_reviews <- function(
    review_ids,
    scope,
    level,
    scope_note = ""
  ) {
    raw_subset <- raw_formal |>
      filter(
        `Review ID` %in%
          review_ids
      )

    adjusted_subset <-
      adjusted_formal |>
      filter(
        `Review ID` %in%
          review_ids
      ) |>
      distinct(
        publication_id,
        `Review ID`
      )

    c_reviews <- n_distinct(
      raw_subset$`Review ID`
    )
    r_publications <- n_distinct(
      adjusted_subset$publication_id
    )
    N_occurrences <- nrow(
      adjusted_subset
    )

    denominator <-
      r_publications * c_reviews -
      r_publications

    tibble(
      Level = level,
      Scope = scope,
      Reviews = c_reviews,
      `Adjusted unique publications` =
        r_publications,
      `Adjusted occurrences` =
        N_occurrences,
      CCA = ifelse(
        denominator > 0,
        (N_occurrences - r_publications) /
          denominator,
        NA_real_
      ),
      `CCA percent` =
        CCA * 100,
      Interpretation = case_when(
        is.na(CCA) ~
          "Not estimable",
        CCA <= 0.05 ~
          "Slight overlap",
        CCA <= 0.10 ~
          "Moderate overlap",
        CCA <= 0.15 ~
          "High overlap",
        TRUE ~
          "Very high overlap"
      ),
      `Scope note` =
        scope_note
    )
  }

  formal_raw_review_ids <- unique(
    raw_formal$`Review ID`
  )

  cca_overall <-
    calc_adjusted_cca_for_reviews(
      formal_raw_review_ids,
      "Tier 1 + Tier 2 formal evidence base",
      "Overall",
      "Exact publication-level CCA after adjudicating all repeated author-year keys."
    )

  tier_sets <- formal_review_meta |>
    filter(
      `Review ID` %in%
        formal_raw_review_ids
    ) |>
    group_by(
      review_tier
    ) |>
    summarise(
      review_ids =
        list(`Review ID`),
      .groups = "drop"
    )

  cca_by_tier <- map2_dfr(
    tier_sets$review_ids,
    tier_sets$review_tier,
    ~ calc_adjusted_cca_for_reviews(
      .x,
      .y,
      "Tier",
      "Exact publication-level CCA within the formal tier."
    )
  )

  domain_sets <- formal_review_meta |>
    filter(
      `Review ID` %in%
        formal_raw_review_ids
    ) |>
    group_by(
      evidence_domain
    ) |>
    summarise(
      review_ids =
        list(`Review ID`),
      .groups = "drop"
    )

  cca_by_domain <- map2_dfr(
    domain_sets$review_ids,
    domain_sets$evidence_domain,
    ~ calc_adjusted_cca_for_reviews(
      .x,
      .y,
      "Evidence-domain review set",
      paste(
        "Review-set CCA proxy: all primary publications from reviews assigned to this evidence domain.",
        "Primary studies were not linked to individual outcome rows."
      )
    )
  )

  review_comparisons <- effects |>
    filter(
      inference_action !=
        "exclude_duplicate",
      comparison_id != "C00"
    ) |>
    distinct(
      `Review ID`,
      comparison_id
    )

  comparison_sets <-
    review_comparisons |>
    filter(
      `Review ID` %in%
        formal_raw_review_ids
    ) |>
    group_by(
      comparison_id
    ) |>
    summarise(
      review_ids =
        list(
          unique(`Review ID`)
        ),
      .groups = "drop"
    )

  cca_by_comparison <-
    map2_dfr(
      comparison_sets$review_ids,
      comparison_sets$comparison_id,
      ~ calc_adjusted_cca_for_reviews(
        .x,
        label_code(
          .y,
          comparison_labels
        ),
        "Comparison review-set proxy",
        paste(
          "Review-set CCA proxy: calculated across all primary publications contained in reviews contributing to this comparison.",
          "It is not a study-to-comparison linkage."
        )
      )
    )

  cca_all <- bind_rows(
    cca_overall,
    cca_by_tier,
    cca_by_domain,
    cca_by_comparison
  )

  # ==================================================================
  # 6. EXACTLY REPRODUCE ALL THREE FROZEN CCA ROWS.
  # ==================================================================
  frozen_compare <- cca_frozen |>
    transmute(
      Scope,
      frozen_reviews =
        as.numeric(
          `Reviews contributing publications`
        ),
      frozen_raw_unique =
        as.numeric(
          `Raw unique author-year keys`
        ),
      frozen_adjusted_unique =
        as.numeric(
          `Adjusted unique primary publications`
        ),
      frozen_raw_occurrences =
        as.numeric(
          `Raw total occurrences`
        ),
      frozen_adjusted_occurrences =
        as.numeric(
          `Adjusted total occurrences`
        ),
      frozen_cca =
        as.numeric(CCA)
    )

  current_tier_compare <-
    cca_by_tier |>
    mutate(
      frozen_scope = case_when(
        str_detect(
          Scope,
          "^Tier 1"
        ) ~ "Tier 1 core",
        str_detect(
          Scope,
          "^Tier 2"
        ) ~
          "Tier 2 contextual formal",
        TRUE ~ NA_character_
      )
    ) |>
    filter(
      !is.na(frozen_scope)
    )

  current_compare <- bind_rows(
    current_tier_compare |>
      transmute(
        frozen_scope,
        current_reviews = Reviews,
        current_adjusted_unique =
          `Adjusted unique publications`,
        current_adjusted_occurrences =
          `Adjusted occurrences`,
        current_cca = CCA
      ),
    cca_overall |>
      transmute(
        frozen_scope =
          "Tier 1 + Tier 2 formal evidence base",
        current_reviews = Reviews,
        current_adjusted_unique =
          `Adjusted unique publications`,
        current_adjusted_occurrences =
          `Adjusted occurrences`,
        current_cca = CCA
      )
  )

  # Raw tier counts come directly from the same detected complete matrix.
  tier_raw_counts <- map2_dfr(
    tier_sets$review_ids,
    tier_sets$review_tier,
    function(ids, tier_name) {
      tmp <- raw_formal |>
        filter(
          `Review ID` %in%
            ids
        )
      tibble(
        frozen_scope = case_when(
          str_detect(
            tier_name,
            "^Tier 1"
          ) ~ "Tier 1 core",
          str_detect(
            tier_name,
            "^Tier 2"
          ) ~
            "Tier 2 contextual formal",
          TRUE ~ NA_character_
        ),
        current_raw_unique =
          n_distinct(
            tmp$author_year_key
          ),
        current_raw_occurrences =
          nrow(
            distinct(
              tmp,
              author_year_key,
              `Review ID`
            )
          )
      )
    }
  ) |>
    filter(
      !is.na(frozen_scope)
    )

  overall_raw_count <- tibble(
    frozen_scope =
      "Tier 1 + Tier 2 formal evidence base",
    current_raw_unique =
      raw_formal_keys,
    current_raw_occurrences =
      raw_formal_occurrences
  )

  current_compare <- current_compare |>
    left_join(
      bind_rows(
        tier_raw_counts,
        overall_raw_count
      ),
      by = "frozen_scope"
    )

  cca_reproduction <- frozen_compare |>
    inner_join(
      current_compare,
      by = c(
        "Scope" =
          "frozen_scope"
      )
    ) |>
    mutate(
      reviews_match =
        frozen_reviews ==
        current_reviews,
      raw_unique_match =
        frozen_raw_unique ==
        current_raw_unique,
      adjusted_unique_match =
        frozen_adjusted_unique ==
        current_adjusted_unique,
      raw_occurrences_match =
        frozen_raw_occurrences ==
        current_raw_occurrences,
      adjusted_occurrences_match =
        frozen_adjusted_occurrences ==
        current_adjusted_occurrences,
      cca_match =
        abs(
          frozen_cca -
          current_cca
        ) < 1e-12,
      all_match =
        reviews_match &
        raw_unique_match &
        adjusted_unique_match &
        raw_occurrences_match &
        adjusted_occurrences_match &
        cca_match
    )

  if (
    nrow(cca_reproduction) != 3L ||
    !all(
      cca_reproduction$all_match
    )
  ) {
    write_csv(
      cca_reproduction,
      "derived/CCA_REPRODUCTION_FAILURE.csv"
    )
    stop(
      "Publication-level reconstruction failed to reproduce one or more frozen CCA rows. ",
      "See derived/CCA_REPRODUCTION_FAILURE.csv",
      call. = FALSE
    )
  }

  # ==================================================================
  # 7. PAIRWISE JACCARD ON THE COMPLETE ADJUSTED PUBLICATION SETS.
  # ==================================================================
  review_universe <- sort(
    unique(
      raw_formal$`Review ID`
    )
  )

  review_sets <- setNames(
    map(
      review_universe,
      function(rid) {
        adjusted_formal |>
          filter(
            `Review ID` == rid
          ) |>
          pull(
            publication_id
          ) |>
          unique() |>
          sort()
      }
    ),
    review_universe
  )

  pair_index <- if (
    length(review_universe) >= 2L
  ) {
    combn(
      review_universe,
      2,
      simplify = FALSE
    )
  } else {
    list()
  }

  pairwise_jaccard <- map_dfr(
    pair_index,
    function(pair) {
      a <- review_sets[[
        pair[[1]]
      ]]
      b <- review_sets[[
        pair[[2]]
      ]]

      intersection_n <-
        length(
          intersect(a,b)
        )
      union_n <-
        length(
          union(a,b)
        )

      tibble(
        Review_A =
          pair[[1]],
        Review_B =
          pair[[2]],
        Publications_A =
          length(a),
        Publications_B =
          length(b),
        Intersection =
          intersection_n,
        Union =
          union_n,
        Jaccard =
          ifelse(
            union_n > 0,
            intersection_n /
              union_n,
            NA_real_
          )
      )
    }
  ) |>
    arrange(
      desc(Jaccard),
      desc(Intersection)
    )

  jaccard_plot_data <-
    pairwise_jaccard |>
    filter(
      Intersection > 0
    ) |>
    slice_head(
      n = 60
    ) |>
    mutate(
      Pair =
        paste(
          Review_A,
          Review_B,
          sep = " × "
        ),
      Pair =
        fct_reorder(
          Pair,
          Jaccard
        )
    )

  if (
    nrow(
      jaccard_plot_data
    )
  ) {
    p_jaccard <- ggplot(
      jaccard_plot_data,
      aes(
        Jaccard,
        Pair,
        size = Intersection,
        colour = Jaccard
      )
    ) +
      geom_point() +
      scale_colour_gradient(
        low = "#9DC3E6",
        high = "#17365D"
      ) +
      labs(
        title =
          "Pairwise primary-publication overlap between reviews",
        subtitle =
          "Complete publication-level matrix after repeated author-year adjudication",
        x =
          "Jaccard similarity",
        y = NULL,
        size =
          "Shared publications",
        colour =
          "Jaccard"
      ) +
      theme_moyamoya(9)

    save_plot_multiformat(
      p_jaccard,
      "results/figures/supplement/Figure_S_pairwise_jaccard",
      9, 12
    )
  }

  # ==================================================================
  # 8. BIPARTITE SHARED-PUBLICATION DISPLAY USING ONLY GGPLOT2.
  # ==================================================================
  shared_publications <-
    adjusted_formal |>
    distinct(
      publication_id,
      `Review ID`
    ) |>
    count(
      publication_id,
      name =
        "Review degree"
    ) |>
    filter(
      `Review degree` >= 2
    )

  network_edges <-
    adjusted_formal |>
    semi_join(
      shared_publications,
      by =
        "publication_id"
    ) |>
    distinct(
      publication_id,
      `Review ID`
    ) |>
    transmute(
      review_id =
        `Review ID`,
      publication_id
    )

  if (nrow(network_edges)) {
    review_positions <-
      network_edges |>
      distinct(
        review_id
      ) |>
      arrange(
        review_id
      ) |>
      mutate(
        x_review = 0,
        y_review =
          seq_len(n())
      )

    publication_positions <-
      network_edges |>
      count(
        publication_id,
        name = "degree"
      ) |>
      arrange(
        desc(degree),
        publication_id
      ) |>
      mutate(
        x_publication = 1,
        y_publication =
          seq(
            1,
            nrow(
              review_positions
            ),
            length.out = n()
          )
      )

    network_plot_data <-
      network_edges |>
      left_join(
        review_positions,
        by = "review_id"
      ) |>
      left_join(
        publication_positions,
        by =
          "publication_id"
      )

    p_network <-
      ggplot() +
      geom_segment(
        data =
          network_plot_data,
        aes(
          x = x_review,
          y = y_review,
          xend =
            x_publication,
          yend =
            y_publication
        ),
        alpha = 0.18,
        colour =
          "#7F8C8D",
        linewidth = 0.35
      ) +
      geom_point(
        data =
          review_positions,
        aes(
          x = x_review,
          y = y_review
        ),
        size = 3.2,
        colour =
          "#17365D"
      ) +
      geom_text(
        data =
          review_positions,
        aes(
          x =
            x_review - 0.025,
          y = y_review,
          label =
            review_id
        ),
        hjust = 1,
        size = 2.8,
        colour =
          "#17365D"
      ) +
      geom_point(
        data =
          publication_positions,
        aes(
          x =
            x_publication,
          y =
            y_publication,
          size = degree
        ),
        colour =
          "#9DC3E6",
        alpha = 0.85
      ) +
      scale_size_continuous(
        range =
          c(1.5,5.5)
      ) +
      scale_x_continuous(
        breaks = c(0,1),
        labels = c(
          "Reviews",
          "Shared primary publications"
        ),
        limits =
          c(-0.18,1.08)
      ) +
      labs(
        title =
          "Review–primary-publication overlap network",
        subtitle =
          "Publication identities after repeated author-year adjudication",
        x = NULL,
        y = NULL,
        size =
          "Review degree"
      ) +
      theme_moyamoya(9) +
      theme(
        axis.text.y =
          element_blank(),
        axis.ticks.y =
          element_blank(),
        panel.grid =
          element_blank()
      )

    save_plot_multiformat(
      p_network,
      "results/figures/supplement/Figure_S_overlap_network",
      13, 10
    )
  }

  # ==================================================================
  # 9. COHORT AND EDITORIAL UPDATE FAMILIES.
  # ==================================================================
  cohort_review_map <-
    cohort_families |>
    select(
      `Cohort family ID`,
      `First author`,
      Years,
      `Primary publication IDs`,
      Reviews,
      Priority,
      `Potential overlap mechanism`,
      `Manuscript handling`
    ) |>
    separate_rows(
      Reviews,
      sep = ",\\s*"
    ) |>
    rename(
      `Review ID` =
        Reviews
    )

  family_text <- reviews |>
    filter(
      `Formal umbrella unit` ==
        "Yes",
      !str_detect(
        str_to_lower(
          coalesce(
            `Editorial family/update`,
            ""
          )
        ),
        "^no explicit|^none|^nr$|^$"
      )
    ) |>
    mutate(
      Year =
        suppressWarnings(
          as.numeric(Year)
        )
    ) |>
    group_by(
      `Editorial family/update`
    ) |>
    arrange(
      desc(Year),
      .by_group = TRUE
    ) |>
    mutate(
      latest_in_family =
        row_number() == 1,
      family_size = n()
    ) |>
    ungroup()

  latest_review_filter <-
    reviews |>
    filter(
      `Formal umbrella unit` ==
        "Yes"
    ) |>
    mutate(
      Year =
        suppressWarnings(
          as.numeric(Year)
        )
    ) |>
    left_join(
      family_text |>
        select(
          `Review ID`,
          latest_in_family,
          family_size
        ),
      by = "Review ID"
    ) |>
    mutate(
      latest_in_family =
        coalesce(
          latest_in_family,
          TRUE
        ),
      family_size =
        coalesce(
          family_size,
          1L
        )
    ) |>
    select(
      `Review ID`,
      latest_in_family,
      family_size
    )

  overlap_method_note <- tibble(
    Item = c(
      "Matrix boundary detection",
      "Full raw overlap matrix",
      "Repeated-key adjudication",
      "Global/tier CCA",
      "Domain/comparison CCA",
      "Pairwise Jaccard",
      "Cohort-level limitation"
    ),
    Method = c(
      paste0(
        "Matrix boundaries detected by literal header/audit markers at source rows ",
        header_row,
        " and ",
        audit_row,
        "; no skip/n_max offsets are used."
      ),
      "482 normalized first-author/year keys from the complete source matrix; each declared review count was checked against its X marks.",
      "All repeated keys replaced by frozen publication-level identities; false publisher/reporting tokens removed and same-author/year collisions split.",
      "Exact publication-level CCA. The complete reconstruction must reproduce every frozen global/tier count and CCA before downstream analysis continues.",
      "Review-set overlap proxy. Primary publications are not linked to individual outcome rows, so these are not presented as outcome-specific CCA.",
      "Calculated from complete adjusted publication sets for each review, including singleton publication proxies.",
      "Publication identity cannot by itself resolve institutional patient-level cohort reuse; high-priority cohort families remain a qualitative and sensitivity concern."
    )
  )

  outputs <- list(
    overlap_matrix_structure_check =
      membership_check,
    overlap_global_count_check =
      global_count_check,
    full_raw_author_year_occurrences =
      raw_occurrences,
    full_adjusted_publication_occurrences =
      adjusted_occurrences,
    removed_overlap_occurrences =
      removed_occurrences,
    cca_all_scopes =
      cca_all,
    cca_frozen =
      cca_frozen,
    cca_reproduction_check =
      cca_reproduction,
    pairwise_jaccard =
      pairwise_jaccard,
    shared_primary_publications =
      shared_publications,
    cohort_families =
      cohort_families,
    cohort_family_review_map =
      cohort_review_map,
    editorial_review_families =
      family_text,
    latest_review_family_filter =
      latest_review_filter,
    overlap_method_note =
      overlap_method_note
  )

  walk2(
    outputs,
    names(outputs),
    ~ write_csv_and_rds(
      .x,
      file.path(
        "derived",
        .y
      )
    )
  )

  safe_write_xlsx(
    outputs,
    "results/tables/08_overlap_tables.xlsx"
  )

  overlap_summary <- list(
    module =
      "08_overlap",
    matrix_header_row =
      header_row,
    matrix_audit_boundary_row =
      audit_row,
    matrix_data_rows =
      nrow(raw_matrix),
    raw_author_year_keys =
      raw_key_count,
    raw_all_occurrences =
      nrow(raw_occurrences),
    raw_formal_unique_keys =
      raw_formal_keys,
    raw_formal_occurrences =
      raw_formal_occurrences,
    adjusted_formal_publications =
      adjusted_formal_publications,
    adjusted_formal_occurrences =
      adjusted_formal_occurrences,
    formal_reviews_contributing_raw_keys =
      raw_formal_reviews,
    frozen_CCA_reproduced =
      all(
        cca_reproduction$all_match
      ),
    overall_CCA =
      cca_overall$CCA[[1]],
    overall_CCA_percent =
      cca_overall$`CCA percent`[[1]],
    review_pairs =
      nrow(pairwise_jaccard),
    pairs_with_overlap =
      sum(
        pairwise_jaccard$Intersection >
          0,
        na.rm = TRUE
      ),
    cohort_families =
      nrow(cohort_families),
    status =
      "PASS"
  )

  write_json(
    overlap_summary,
    "derived/overlap_analysis_summary.json",
    pretty = TRUE,
    auto_unbox = TRUE
  )

  message(
    "Overlap module completed."
  )
  message(
    "Complete matrix rows/keys: ",
    nrow(raw_matrix),
    "/",
    raw_key_count
  )
  message(
    "Formal raw keys/occurrences/reviews: ",
    raw_formal_keys,
    "/",
    raw_formal_occurrences,
    "/",
    raw_formal_reviews
  )
  message(
    "Formal adjusted publications/occurrences: ",
    adjusted_formal_publications,
    "/",
    adjusted_formal_occurrences
  )
  message(
    "Reproduced formal CCA: ",
    sprintf(
      "%.9f%%",
      cca_overall$`CCA percent`[[1]]
    )
  )
})

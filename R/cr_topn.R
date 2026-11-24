#' Describe top N registry areas by ASR
#'
#' `cr_topn()` creates a paragraph describing the top N registry areas by a
#' selected standardized rate for total, male, and female strata.
#'
#' @param data A data frame with ASR columns such as `areacode`, `areacode_cn`,
#'   `areacode_en`, `sex`, `cancer`, `cancer_cn`, `cancer_en`, and standardized
#'   rate columns.
#' @param cancer Cancer code, Chinese cancer label, or English cancer label.
#' @param event_type Event type. One of `"incidence"` or `"mortality"`.
#' @param areas Optional area codes or labels used to limit areas included in the
#'   ranking. Values can match `areacode`, `areacode_cn`, or `areacode_en`.
#' @param area_scope Optional language-specific name describing the area scope,
#'   without the area count. If `NULL`, a default registry area name is used.
#' @param include_area_scope Whether to include the area scope phrase at the
#'   beginning of the first sentence.
#' @param n Number of top areas to report.
#' @param rate_col Column used for ranking and displayed values. Defaults to
#'   `"asr_cn2000"`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param sex_specific Whether the cancer should be described without sex
#'   stratification. If `NULL`, this is inferred from sex-specific case counts.
#' @param digits Number of decimal places for rates.
#'
#' @return A character string.
#' @export
cr_topn <- function(data,
                    cancer,
                    event_type = c("incidence", "mortality"),
                    areas = NULL,
                    area_scope = NULL,
                    include_area_scope = TRUE,
                    n = 5,
                    rate_col = "asr_cn2000",
                    lang = "zh",
                    sex_specific = NULL,
                    digits = 2) {
  event_type <- match.arg(event_type)
  lang <- match.arg(lang, c("zh", "en"))

  .cr_validate_topn_data(data, rate_col = rate_col)
  .cr_validate_topn_args(n = n, digits = digits, include_area_scope = include_area_scope)

  template <- .cr_load_template("topn", lang = lang)
  lang_data <- .cr_load_lang(lang)
  event_terms <- .cr_get_event_terms(event_type, lang = lang)
  label_suffix <- .cr_label_suffix(lang)

  rows <- .cr_filter_topn_rows(data, cancer = cancer, areas = areas)
  cancer_name <- .cr_label_value(rows[1L, , drop = FALSE], "cancer", label_suffix)

  if (is.null(sex_specific)) {
    sex_specific <- .cr_topn_is_sex_specific(rows)
  }
  sex_specific <- isTRUE(sex_specific)

  total_topn <- .cr_rank_topn_rows(rows, sex = 0, n = n, rate_col = rate_col)
  male_topn <- if (sex_specific) NULL else .cr_rank_topn_rows(rows, sex = 1, n = n, rate_col = rate_col)
  female_topn <- if (sex_specific) NULL else .cr_rank_topn_rows(rows, sex = 2, n = n, rate_col = rate_col)

  values <- list(
    area_scope = .cr_topn_area_scope(rows, areas = areas, area_scope = area_scope, lang_data = lang_data),
    cancer_name = cancer_name,
    cancer_name_lower = tolower(cancer_name),
    event_noun = event_terms$noun,
    event_rate_label = event_terms$rate_label,
    standardized_rate_label = .cr_topn_standardized_rate_label(event_terms, lang_data, lang),
    sex_male = lang_data$sex$male,
    sex_female = lang_data$sex$female,
    phrase_asr_china = lang_data$phrase$asr_china,
    phrase_per_100000 = lang_data$phrase$per_100000,
    total_area_names = .cr_topn_area_names(total_topn, label_suffix = label_suffix, lang_data = lang_data, lang = lang),
    total_rate_values = .cr_topn_rate_values(total_topn[[rate_col]], digits = digits, lang_data = lang_data, lang = lang),
    male_area_names = if (sex_specific) "" else .cr_topn_area_names(male_topn, label_suffix = label_suffix, lang_data = lang_data, lang = lang),
    male_rate_values = if (sex_specific) "" else .cr_topn_rate_values(male_topn[[rate_col]], digits = digits, lang_data = lang_data, lang = lang),
    female_area_names = if (sex_specific) "" else .cr_topn_area_names(female_topn, label_suffix = label_suffix, lang_data = lang_data, lang = lang),
    female_rate_values = if (sex_specific) "" else .cr_topn_rate_values(female_topn[[rate_col]], digits = digits, lang_data = lang_data, lang = lang)
  )

  template_key <- .cr_topn_template_key(
    sex_specific = sex_specific,
    include_area_scope = include_area_scope
  )
  .cr_render_text(template$overall[[template_key]], values)
}

.cr_validate_topn_data <- function(data, rate_col) {
  required <- c("areacode", "areacode_en", "areacode_cn", "sex", "cancer", "cancer_en", "cancer_cn", "no_cases")
  .cr_check_required_columns(data, required)
  if (!is.character(rate_col) || length(rate_col) != 1L || is.na(rate_col)) {
    stop("`rate_col` must be a single column name.", call. = FALSE)
  }
  if (!rate_col %in% names(data)) {
    stop("`rate_col` is not found in `data`: ", rate_col, call. = FALSE)
  }
  if (!is.numeric(data[[rate_col]])) {
    stop("`rate_col` must identify a numeric column.", call. = FALSE)
  }
  invisible(data)
}

.cr_validate_topn_args <- function(n, digits, include_area_scope) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L || n != as.integer(n)) {
    stop("`n` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0L) {
    stop("`digits` must be a non-negative number.", call. = FALSE)
  }
  if (!is.logical(include_area_scope) || length(include_area_scope) != 1L || is.na(include_area_scope)) {
    stop("`include_area_scope` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.cr_topn_template_key <- function(sex_specific, include_area_scope) {
  if (sex_specific && include_area_scope) {
    "sex_specific"
  } else if (sex_specific) {
    "sex_specific_no_scope"
  } else if (include_area_scope) {
    "default"
  } else {
    "default_no_scope"
  }
}

.cr_filter_topn_rows <- function(data, cancer, areas = NULL) {
  cancer_values <- as.character(cancer)
  keep <- as.character(data$cancer) %in% cancer_values |
    as.character(data$cancer_cn) %in% cancer_values |
    as.character(data$cancer_en) %in% cancer_values

  if (!is.null(areas)) {
    area_values <- as.character(areas)
    keep <- keep & (
      as.character(data$areacode) %in% area_values |
        as.character(data$areacode_cn) %in% area_values |
        as.character(data$areacode_en) %in% area_values
    )
  }

  rows <- data[keep & as.character(data$sex) %in% c("0", "1", "2"), , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop("No ASR rows found for the requested cancer and areas.", call. = FALSE)
  }
  missing_sex <- setdiff(c("0", "1", "2"), unique(as.character(rows$sex)))
  if (length(missing_sex) > 0L) {
    stop("ASR rows are missing required sex strata: ", paste(missing_sex, collapse = ", "), call. = FALSE)
  }
  rows
}

.cr_topn_is_sex_specific <- function(rows) {
  male_cases <- sum(rows$no_cases[as.character(rows$sex) == "1"], na.rm = TRUE)
  female_cases <- sum(rows$no_cases[as.character(rows$sex) == "2"], na.rm = TRUE)
  isTRUE(male_cases == 0) || isTRUE(female_cases == 0)
}

.cr_rank_topn_rows <- function(rows, sex, n, rate_col) {
  sex_rows <- rows[as.character(rows$sex) == as.character(sex), , drop = FALSE]
  sex_rows <- sex_rows[!is.na(sex_rows[[rate_col]]), , drop = FALSE]
  sex_rows <- sex_rows[order(-sex_rows[[rate_col]], as.character(sex_rows$areacode)), , drop = FALSE]
  if (nrow(sex_rows) == 0L) {
    stop("No non-missing rate values found for sex = ", sex, ".", call. = FALSE)
  }
  utils::head(sex_rows, n)
}

.cr_topn_area_scope <- function(rows, areas = NULL, area_scope = NULL, lang_data) {
  n_areas <- length(unique(rows$areacode[as.character(rows$sex) == "0"]))
  scope_name <- if (is.null(area_scope)) {
    lang_data$phrase$cancer_registration_areas
  } else {
    as.character(area_scope)
  }

  if (isTRUE(lang_data$phrase$cancer_registration_area_prefix)) {
    paste(n_areas, scope_name)
  } else {
    paste0(n_areas, lang_data$phrase$area_count_suffix, scope_name)
  }
}

.cr_topn_standardized_rate_label <- function(event_terms, lang_data, lang) {
  if (identical(lang, "en")) {
    paste(event_terms$noun, "of", lang_data$phrase$asr_china)
  } else {
    paste0(lang_data$phrase$standardized, event_terms$rate_label)
  }
}

.cr_topn_area_names <- function(rows, label_suffix, lang_data, lang) {
  area_names <- .cr_label_value(rows, "areacode", label_suffix)
  if (identical(lang, "en")) .cr_collapse_topn_names_en(area_names, lang_data) else .cr_collapse_topn_names_zh(area_names, lang_data)
}

.cr_topn_rate_values <- function(x, digits, lang_data, lang) {
  values <- .cr_format_vector(x, digits = digits)
  if (identical(lang, "en")) {
    .cr_collapse_en(paste(values, lang_data$phrase$per_100000), lang_data)
  } else {
    .cr_collapse_zh(paste0(values, lang_data$phrase$per_100000), lang_data)
  }
}

.cr_collapse_topn_names_zh <- function(x, lang_data) {
  x <- as.character(x)
  if (length(x) == 0L) return("")
  if (length(x) == 1L) return(x)
  paste0(x[1L], lang_data$list$comma, lang_data$list$followed_by, .cr_collapse_zh(x[-1L], lang_data))
}

.cr_collapse_topn_names_en <- function(x, lang_data) {
  x <- as.character(x)
  if (length(x) == 0L) return("")
  if (length(x) == 1L) return(x)
  paste0(x[1L], lang_data$list$followed_by, .cr_collapse_en(x[-1L], lang_data))
}


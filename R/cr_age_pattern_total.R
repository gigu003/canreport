#' Describe age-specific rate patterns for all cancers
#'
#' `cr_age_pattern_total()` creates report-style paragraphs describing the
#' age-specific rate pattern for total malignant tumors in one registration area
#' and one event type from an age-specific rate table.
#'
#' @param data A data frame with age-specific rate columns such as `areacode`,
#'   `areacode_cn`, `areacode_en`, `year`, `sex`, `cancer`, `cancer_cn`,
#'   `cancer_en`, `agegrp`, `cases`, and `rate`.
#' @param area Main area code or label used for total, male, and female rows.
#' @param cancer Cancer code, Chinese cancer label, or English cancer label for
#'   the total malignant tumor row, for example `60`.
#' @param event_type Event type. One of `"incidence"` or `"mortality"`.
#' @param area_roles A named list identifying urban and rural rows. Defaults to
#'   `list(urban = 910000, rural = 920000)`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param low_rate_threshold Relative threshold for detecting the initial low
#'   rate age span. Consecutive age groups from the youngest age group with
#'   `rate <= peak_rate * low_rate_threshold` are treated as low-level ages.
#' @param trend_similarity_threshold Spearman correlation threshold for treating
#'   age-specific rate trends as similar.
#' @param compare_threshold Relative threshold for treating two age-specific
#'   rates as similar in sex and urban-rural comparisons.
#' @param obvious_threshold Relative threshold for describing urban-rural
#'   differences as obvious rather than slight.
#' @param digits Number of decimal places for rates.
#'
#' @return A character string.
#' @export
cr_age_pattern_total <- function(data,
                                 area,
                                 cancer,
                                 event_type = c("incidence", "mortality"),
                                 area_roles = list(urban = 910000, rural = 920000),
                                 lang = "zh",
                                 low_rate_threshold = 0.05,
                                 trend_similarity_threshold = 0.8,
                                 compare_threshold = 0,
                                 obvious_threshold = 0.2,
                                 digits = 2) {
  event_type <- match.arg(event_type)
  lang <- match.arg(lang, c("zh", "en"))

  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }
  if (missing(cancer) || is.null(cancer)) {
    stop("`cancer` must be supplied.", call. = FALSE)
  }

  .cr_validate_age_pattern_data(data)
  .cr_validate_age_pattern_args(
    low_rate_threshold = low_rate_threshold,
    sex_dominance_threshold = 0,
    trend_similarity_threshold = trend_similarity_threshold,
    area_difference_threshold = compare_threshold,
    digits = digits
  )
  .cr_check_unit_interval(obvious_threshold, "obvious_threshold")

  template <- .cr_load_template("age_pattern_total", lang = lang)
  lang_data <- .cr_load_lang(lang)
  event_terms <- .cr_get_event_terms(event_type, lang = lang)
  label_suffix <- .cr_label_suffix(lang)

  main_total <- .cr_pick_age_rows(data, cancer = cancer, area = area, sex = 0)
  main_male <- .cr_pick_age_rows(data, cancer = cancer, area = area, sex = 1)
  main_female <- .cr_pick_age_rows(data, cancer = cancer, area = area, sex = 2)
  urban_total <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$urban, sex = 0)
  urban_male <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$urban, sex = 1)
  urban_female <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$urban, sex = 2)
  rural_total <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$rural, sex = 0)
  rural_male <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$rural, sex = 1)
  rural_female <- .cr_pick_age_rows(data, cancer = cancer, area = area_roles$rural, sex = 2)

  peak <- .cr_age_pattern_peak(main_total)
  low_info <- .cr_age_pattern_total_low_info(
    main_total,
    peak_rate = peak$rate,
    threshold = low_rate_threshold,
    lang = lang,
    lang_data = lang_data
  )
  sex_detail <- .cr_age_pattern_total_sex_detail(
    main_male,
    main_female,
    threshold = compare_threshold,
    trend_similarity_threshold = trend_similarity_threshold,
    lang = lang,
    lang_data = lang_data
  )
  area_detail <- .cr_age_pattern_total_area_detail(
    urban_total,
    rural_total,
    urban_male,
    rural_male,
    urban_female,
    rural_female,
    threshold = compare_threshold,
    obvious_threshold = obvious_threshold,
    trend_similarity_threshold = trend_similarity_threshold,
    event_rate_label = event_terms$rate_label,
    lang = lang,
    lang_data = lang_data
  )

  values <- list(
    year = main_total$year[1L],
    area_name = .cr_label_value(main_total[1L, , drop = FALSE], "areacode", label_suffix),
    cancer_name = .cr_label_value(main_total[1L, , drop = FALSE], "cancer", label_suffix),
    cancer_name_lower = tolower(.cr_label_value(main_total[1L, , drop = FALSE], "cancer", label_suffix)),
    event_noun = event_terms$noun,
    event_rate_label = event_terms$rate_label,
    age_specific_rate_label = .cr_age_specific_rate_label(event_terms, lang = lang, lang_data = lang_data),
    phrase_per_100000 = lang_data$phrase$per_100000,
    low_age_span = low_info$low_age_span,
    rise_start_age = low_info$rise_start_age,
    peak_age_group = .cr_clean_age_group(peak$agegrp, lang = lang, lang_data = lang_data),
    peak_rate = .cr_format_number(peak$rate, digits = digits),
    sex_trend_template = template$sex_trend[[sex_detail$trend_key]],
    female_higher_age_spans = sex_detail$female_higher_age_spans,
    male_higher_age_spans = sex_detail$male_higher_age_spans,
    area_sex_trend_template = template$area_sex_trend[[area_detail$sex_trend_key]],
    area_comparison_separator = if (identical(lang, "en")) "; " else lang_data$list$comma,
    area_sex_distribution_template = template$area_sex_distribution[[area_detail$sex_distribution_key]]
  )

  values$area_comparison_text <- .cr_age_pattern_total_render_area_comparison(
    area_detail$comparison,
    template = template,
    values = values
  )
  values$sex_trend_text <- .cr_render_text(values$sex_trend_template, values)
  values$area_sex_trend_text <- .cr_render_text(values$area_sex_trend_template, values)
  values$area_sex_distribution_text <- .cr_render_text(values$area_sex_distribution_template, values)

  .cr_render_text(template$overall$default, values)
}

.cr_age_pattern_total_low_info <- function(rows, peak_rate, threshold, lang, lang_data) {
  cutoff <- peak_rate * threshold
  is_low <- !is.na(rows$rate) & rows$rate <= cutoff
  end <- if (any(is_low) && isTRUE(is_low[1L])) rle(is_low)$lengths[1L] else 1L
  first_age <- .cr_age_group_lower(rows$agegrp[1L])
  last_age <- .cr_age_group_upper(rows$agegrp[end])
  rise_index <- min(end + 1L, nrow(rows))
  list(
    low_age_span = .cr_age_span_label(first_age, last_age, lang = lang, lang_data = lang_data),
    rise_start_age = .cr_age_group_lower(rows$agegrp[rise_index])
  )
}

.cr_age_pattern_total_sex_detail <- function(male_rows,
                                             female_rows,
                                             threshold,
                                             trend_similarity_threshold,
                                             lang,
                                             lang_data) {
  merged <- .cr_merge_age_rates(male_rows, female_rows, suffixes = c("_male", "_female"))
  merged <- merged[!is.na(merged$rate_male) & !is.na(merged$rate_female), , drop = FALSE]
  if (nrow(merged) < 2L) {
    trend_key <- "different"
  } else {
    trend_cor <- suppressWarnings(stats::cor(merged$rate_male, merged$rate_female, method = "spearman"))
    trend_key <- if (!is.na(trend_cor) && trend_cor >= trend_similarity_threshold) "similar" else "different"
  }

  comparison <- .cr_age_pattern_total_pairwise_status(
    merged$rate_male,
    merged$rate_female,
    left_higher = "male_higher",
    right_higher = "female_higher",
    threshold = threshold
  )
  female_spans <- .cr_age_pattern_total_comparison_spans(merged$agegrp, comparison, "female_higher", lang = lang, lang_data = lang_data)
  male_spans <- .cr_age_pattern_total_comparison_spans(merged$agegrp, comparison, "male_higher", lang = lang, lang_data = lang_data)

  list(
    trend_key = trend_key,
    female_higher_age_spans = female_spans,
    male_higher_age_spans = male_spans
  )
}

.cr_age_pattern_total_area_detail <- function(urban_total,
                                              rural_total,
                                              urban_male,
                                              rural_male,
                                              urban_female,
                                              rural_female,
                                              threshold,
                                              obvious_threshold,
                                              trend_similarity_threshold,
                                              event_rate_label,
                                              lang,
                                              lang_data) {
  total <- .cr_age_pattern_total_area_comparison(urban_total, rural_total, threshold, obvious_threshold, event_rate_label, lang, lang_data)
  male <- .cr_age_pattern_total_area_comparison(urban_male, rural_male, threshold, obvious_threshold, event_rate_label, lang, lang_data)
  female <- .cr_age_pattern_total_area_comparison(urban_female, rural_female, threshold, obvious_threshold, event_rate_label, lang, lang_data)

  sex_trends <- c(
    male = .cr_age_pattern_area_status(urban_male, rural_male, trend_similarity_threshold, threshold),
    female = .cr_age_pattern_area_status(urban_female, rural_female, trend_similarity_threshold, threshold)
  )
  sex_trend_key <- if (all(sex_trends %in% c("similar", "different_similar"))) "similar" else "different"

  sex_distribution_key <- if (identical(male$dominant_key, female$dominant_key)) "similar" else "different"

  list(
    comparison = total,
    sex_trend_key = sex_trend_key,
    sex_distribution_key = sex_distribution_key
  )
}

.cr_age_pattern_total_area_comparison <- function(urban_rows,
                                                 rural_rows,
                                                 threshold,
                                                 obvious_threshold,
                                                 event_rate_label,
                                                 lang,
                                                 lang_data) {
  merged <- .cr_merge_age_rates(urban_rows, rural_rows, suffixes = c("_urban", "_rural"))
  merged <- merged[!is.na(merged$rate_urban) & !is.na(merged$rate_rural), , drop = FALSE]
  if (nrow(merged) == 0L) {
    return(list(keys = "similar", dominant_key = "similar"))
  }

  status <- .cr_age_pattern_total_pairwise_status(
    merged$rate_urban,
    merged$rate_rural,
    left_higher = "urban_higher",
    right_higher = "rural_higher",
    threshold = threshold
  )
  diff <- abs(merged$rate_urban - merged$rate_rural) / pmax(abs(merged$rate_urban), abs(merged$rate_rural), .Machine$double.eps)
  intensity <- ifelse(diff >= obvious_threshold, "obvious", "slight")
  urban_slight <- .cr_age_pattern_total_comparison_spans(merged$agegrp, status == "urban_higher" & intensity == "slight", TRUE, lang = lang, lang_data = lang_data)
  urban_obvious <- .cr_age_pattern_total_comparison_spans(merged$agegrp, status == "urban_higher" & intensity == "obvious", TRUE, lang = lang, lang_data = lang_data)
  rural_higher <- .cr_age_pattern_total_comparison_spans(merged$agegrp, status, "rural_higher", lang = lang, lang_data = lang_data)

  dominant_key <- .cr_age_pattern_area_summary_key(status)
  pieces <- list()
  if (!identical(urban_slight, "")) {
    pieces[[length(pieces) + 1L]] <- list(key = "urban_slight", values = list(urban_slight_age_spans = urban_slight))
  }
  if (!identical(urban_obvious, "")) {
    pieces[[length(pieces) + 1L]] <- list(key = "urban_obvious", values = list(urban_obvious_age_spans = urban_obvious))
  }
  if (!identical(rural_higher, "")) {
    pieces[[length(pieces) + 1L]] <- list(key = "rural_higher", values = list(rural_higher_age_spans = rural_higher))
  }
  if (length(pieces) == 0L) {
    pieces[[1L]] <- list(key = "similar", values = list())
  }
  list(keys = pieces, dominant_key = dominant_key)
}

.cr_age_pattern_total_pairwise_status <- function(left, right, left_higher, right_higher, threshold) {
  mapply(
    .cr_compare_status,
    left = left,
    right = right,
    MoreArgs = list(left_higher = left_higher, right_higher = right_higher, threshold = threshold),
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
}

.cr_age_pattern_total_comparison_spans <- function(agegrp, comparison, target, lang, lang_data) {
  is_target <- comparison == target
  if (!any(is_target)) return("")

  runs <- rle(is_target)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  spans <- character(0L)
  for (i in seq_along(runs$values)) {
    if (!isTRUE(runs$values[[i]])) next
    first_age <- .cr_age_group_lower(agegrp[starts[[i]]])
    last_age <- .cr_age_group_upper(agegrp[ends[[i]]])
    spans <- c(spans, .cr_age_span_label(first_age, last_age, lang = lang, lang_data = lang_data))
  }
  .cr_collapse_age_spans(spans, lang = lang, lang_data = lang_data)
}

.cr_age_pattern_total_render_area_comparison <- function(comparison, template, values) {
  rendered <- vapply(
    comparison$keys,
    function(piece) {
      piece_values <- values
      piece_values[names(piece$values)] <- piece$values
      .cr_render_text(template$area_comparison[[piece$key]], piece_values)
    },
    character(1L)
  )
  paste(rendered, collapse = values$area_comparison_separator)
}
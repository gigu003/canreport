#' Generate an overview paragraph for all cancers
#'
#' `cr_overview_total()` creates an overview paragraph for the total malignant
#' tumor burden in one registration area and one event type from an ASR table.
#'
#' @param data A data frame with ASR columns such as `areacode`, `areacode_cn`,
#'   `areacode_en`, `year`, `sex`, `cancer`, `cancer_cn`, `cancer_en`,
#'   `no_cases`, `cr`, `asr_cn2000`, `asr_wld85`, and `cumur`.
#' @param event_type Event type. One of `"incidence"` or `"mortality"`.
#' @param area Main area code or label used for the total, male, and female
#'   rows.
#' @param area_roles A named list identifying urban and rural rows. Defaults to
#'   `list(urban = 910000, rural = 920000)`.
#' @param cancer_total Optional cancer code or label identifying the all-cancers
#'   row. If `NULL`, total malignant tumor rows are detected automatically.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param digits Number of decimal places for rates and percentages.
#' @param compare_threshold Relative threshold for treating two standardized
#'   rates as similar.
#' @param output Output mode. `"text"` returns the rendered paragraph, `"data"`
#'   returns a one-row data frame of template values, and `"both"` returns both.
#'
#' @return A character string, data frame, or list, depending on `output`.
#' @export
cr_overview_total <- function(data,
                              event_type = c("incidence", "mortality"),
                              area,
                              area_roles = list(urban = 910000, rural = 920000),
                              cancer_total = NULL,
                              lang = "zh",
                              digits = 2,
                              compare_threshold = 0,
                              output = c("text", "data", "both")) {
  event_type <- match.arg(event_type)
  lang <- match.arg(lang, c("zh", "en"))
  output <- .cr_check_output(output)
  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }

  .cr_validate_overview_total_data(data)
  .cr_check_digits(digits)
  .cr_check_threshold(compare_threshold)

  template <- .cr_load_template("overview_total", lang = lang)
  lang_data <- .cr_load_lang(lang)
  zh_lang_data <- .cr_load_lang("zh")
  en_lang_data <- .cr_load_lang("en")
  event_terms <- .cr_get_event_terms(event_type, lang = lang)
  label_suffix <- .cr_label_suffix(lang)

  main_total <- .cr_pick_overview_total_row(data, area = area, sex = 0, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  main_male <- .cr_pick_overview_total_row(data, area = area, sex = 1, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  main_female <- .cr_pick_overview_total_row(data, area = area, sex = 2, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  urban_total <- .cr_pick_overview_total_row(data, area = area_roles$urban, sex = 0, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  urban_male <- .cr_pick_overview_total_row(data, area = area_roles$urban, sex = 1, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  urban_female <- .cr_pick_overview_total_row(data, area = area_roles$urban, sex = 2, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  rural_total <- .cr_pick_overview_total_row(data, area = area_roles$rural, sex = 0, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  rural_male <- .cr_pick_overview_total_row(data, area = area_roles$rural, sex = 1, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  rural_female <- .cr_pick_overview_total_row(data, area = area_roles$rural, sex = 2, cancer_total = cancer_total, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)

  values <- list(
    year = main_total$year,
    area_name = .cr_label_value(main_total, "areacode", label_suffix),
    area_urban_full = lang_data$area$urban_full,
    area_rural_full = lang_data$area$rural_full,
    area_urban_full_sentence = .cr_sentence_case_en(lang_data$area$urban_full),
    area_rural_full_sentence = .cr_sentence_case_en(lang_data$area$rural_full),
    sex_male = lang_data$sex$male,
    sex_female = lang_data$sex$female,
    event_noun = event_terms$noun,
    event_case_label = event_terms$case_label,
    event_rate_label = event_terms$rate_label,
    phrase_per_100000 = lang_data$phrase$per_100000,
    total_cases = .cr_format_number(main_total$no_cases, digits = 0),
    male_cases = .cr_format_number(main_male$no_cases, digits = 0),
    female_cases = .cr_format_number(main_female$no_cases, digits = 0),
    urban_cases = .cr_format_number(urban_total$no_cases, digits = 0),
    rural_cases = .cr_format_number(rural_total$no_cases, digits = 0),
    urban_prop = .cr_format_number(.cr_overview_total_prop(urban_total$no_cases, main_total$no_cases), digits = digits),
    rural_prop = .cr_format_number(.cr_overview_total_prop(rural_total$no_cases, main_total$no_cases), digits = digits),
    total_cr = .cr_format_number(main_total$cr, digits = digits),
    male_cr = .cr_format_number(main_male$cr, digits = digits),
    female_cr = .cr_format_number(main_female$cr, digits = digits),
    total_asr_cn2000 = .cr_format_number(main_total$asr_cn2000, digits = digits),
    total_asr_wld85 = .cr_format_number(main_total$asr_wld85, digits = digits),
    total_cumur = .cr_format_number(main_total$cumur, digits = digits),
    urban_cr = .cr_format_number(urban_total$cr, digits = digits),
    urban_male_cr = .cr_format_number(urban_male$cr, digits = digits),
    urban_female_cr = .cr_format_number(urban_female$cr, digits = digits),
    urban_asr_cn2000 = .cr_format_number(urban_total$asr_cn2000, digits = digits),
    urban_asr_wld85 = .cr_format_number(urban_total$asr_wld85, digits = digits),
    urban_cumur = .cr_format_number(urban_total$cumur, digits = digits),
    rural_cr = .cr_format_number(rural_total$cr, digits = digits),
    rural_male_cr = .cr_format_number(rural_male$cr, digits = digits),
    rural_female_cr = .cr_format_number(rural_female$cr, digits = digits),
    rural_asr_cn2000 = .cr_format_number(rural_total$asr_cn2000, digits = digits),
    rural_asr_wld85 = .cr_format_number(rural_total$asr_wld85, digits = digits),
    rural_cumur = .cr_format_number(rural_total$cumur, digits = digits)
  )

  comparison_key <- .cr_overview_total_comparison_key(
    urban_total = urban_total,
    rural_total = rural_total,
    urban_male = urban_male,
    rural_male = rural_male,
    urban_female = urban_female,
    rural_female = rural_female,
    threshold = compare_threshold
  )
  values$urban_rural_compare_text <- .cr_render_text(template$comparison[[comparison_key]], values)

  text <- .cr_render_text(template$overall$default, values)
  .cr_output_result(text, values, output = output)
}

.cr_validate_overview_total_data <- function(data) {
  required <- c(
    "areacode", "areacode_en", "areacode_cn", "year", "sex", "cancer",
    "cancer_en", "cancer_cn", "no_cases", "cr", "asr_cn2000",
    "asr_wld85", "cumur"
  )
  .cr_check_required_columns(data, required)
  invisible(data)
}

.cr_pick_overview_total_row <- function(data, area, sex, cancer_total, zh_lang_data, en_lang_data) {
  area_values <- as.character(area)
  keep <- as.character(data$sex) == as.character(sex) &
    (
      as.character(data$areacode) %in% area_values |
        as.character(data$areacode_cn) %in% area_values |
        as.character(data$areacode_en) %in% area_values
    )

  if (is.null(cancer_total)) {
    keep <- keep & .cr_overview_total_is_total_cancer(data, zh_lang_data = zh_lang_data, en_lang_data = en_lang_data)
  } else {
    cancer_values <- as.character(cancer_total)
    keep <- keep & (
      as.character(data$cancer) %in% cancer_values |
        as.character(data$cancer_cn) %in% cancer_values |
        as.character(data$cancer_en) %in% cancer_values
    )
  }

  row <- data[keep, , drop = FALSE]
  if (nrow(row) == 0L) {
    stop("No total cancer ASR row found for the requested area and sex.", call. = FALSE)
  }
  if (nrow(row) > 1L) {
    stop("Multiple total cancer ASR rows found for the requested area and sex.", call. = FALSE)
  }
  row
}

.cr_overview_total_is_total_cancer <- function(data, zh_lang_data, en_lang_data) {
  cancer_values <- tolower(as.character(data$cancer))
  cancer_cn <- as.character(data$cancer_cn)
  cancer_en <- tolower(as.character(data$cancer_en))
  cancer_values %in% c("0", "00", "000", "100", "all", "all cancers") |
    grepl(zh_lang_data$spectrum$exclude_cancer_pattern, cancer_cn) |
    grepl(en_lang_data$spectrum$exclude_cancer_pattern, cancer_en)
}

.cr_overview_total_prop <- function(x, total) {
  if (is.na(x) || is.na(total) || total == 0) {
    return(NA_real_)
  }
  x / total * 100
}

.cr_overview_total_comparison_key <- function(urban_total,
                                              rural_total,
                                              urban_male,
                                              rural_male,
                                              urban_female,
                                              rural_female,
                                              threshold) {
  statuses <- c(
    total = .cr_compare_status(urban_total$asr_cn2000, rural_total$asr_cn2000, "urban_higher", "rural_higher", threshold = threshold),
    male = .cr_compare_status(urban_male$asr_cn2000, rural_male$asr_cn2000, "urban_higher", "rural_higher", threshold = threshold),
    female = .cr_compare_status(urban_female$asr_cn2000, rural_female$asr_cn2000, "urban_higher", "rural_higher", threshold = threshold)
  )
  if (all(statuses == "urban_higher")) return("all_urban_higher")
  if (all(statuses == "rural_higher")) return("all_rural_higher")
  if (all(statuses == "similar")) return("all_similar")
  "mixed"
}
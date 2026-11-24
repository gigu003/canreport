#' Describe the composition of cancer subsites
#'
#' `cr_subsite()` creates a paragraph describing the proportion of cases with
#' complete ICD-10 subsite information and the composition of specified subsites
#' for one cancer in one area.
#'
#' @param data A data frame with ICD-10 subsite count columns such as
#'   `areacode`, `areacode_cn`, `areacode_en`, `sex`, `cancer`, `cancer_cn`,
#'   `cancer_en`, `icd10`, `icd10_cn`, `icd10_en`, and `count`.
#' @param cancer Cancer code, Chinese cancer label, or English cancer label.
#' @param area Area code, Chinese area label, or English area label.
#' @param sex Sex stratum. One of `"total"`, `"male"`, `"female"`, `0`, `1`, or
#'   `2`. When `"total"` or `0` is supplied, male and female rows are combined.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param unspecified_pattern Regular expression identifying unspecified
#'   subsites to exclude from complete subsite information. If `NULL`, defaults
#'   to a pattern matching unspecified subsites in Chinese or English.
#' @param digits Number of decimal places for percentages.
#' @param max_subsites Maximum number of complete subsites to report. Defaults
#'   to all complete subsites.
#'
#' @return A character string.
#' @export
cr_subsite <- function(data,
                       cancer,
                       area,
                       sex = "total",
                       lang = "zh",
                       unspecified_pattern = NULL,
                       digits = 2,
                       max_subsites = Inf) {
  lang <- match.arg(lang, c("zh", "en"))
  if (missing(cancer) || is.null(cancer)) {
    stop("`cancer` must be supplied.", call. = FALSE)
  }
  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }

  .cr_validate_subsite_data(data)
  .cr_validate_subsite_args(sex = sex, digits = digits, max_subsites = max_subsites)

  template <- .cr_load_template("subsite", lang = lang)
  lang_data <- .cr_load_lang(lang)
  label_suffix <- .cr_label_suffix(lang)
  rows <- .cr_pick_subsite_rows(data, cancer = cancer, area = area, sex = sex)
  summary <- .cr_subsite_summary(
    rows,
    label_suffix = label_suffix,
    unspecified_pattern = unspecified_pattern,
    digits = digits,
    max_subsites = max_subsites,
    lang = lang,
    lang_data = lang_data
  )

  cancer_name <- .cr_label_value(rows[1L, , drop = FALSE], "cancer", label_suffix)
  values <- list(
    cancer_name = cancer_name,
    cancer_name_lower = tolower(cancer_name),
    known_prop = summary$known_prop,
    top_subsite = summary$top_subsite,
    top_prop = summary$top_prop,
    other_subsites = summary$other_subsites,
    other_props = summary$other_props
  )

  template_key <- if (summary$n_subsites > 1L) "default" else "single"
  .cr_render_text(template$overall[[template_key]], values)
}

.cr_validate_subsite_data <- function(data) {
  required <- c(
    "areacode", "areacode_en", "areacode_cn", "sex", "cancer",
    "cancer_en", "cancer_cn", "icd10", "icd10_en", "icd10_cn", "count"
  )
  .cr_check_required_columns(data, required)
  if (!is.numeric(data$count)) {
    stop("`count` must be numeric.", call. = FALSE)
  }
  invisible(data)
}

.cr_validate_subsite_args <- function(sex, digits, max_subsites) {
  .cr_subsite_sex_code(sex)
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0L) {
    stop("`digits` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(max_subsites) || length(max_subsites) != 1L || is.na(max_subsites) || max_subsites < 1L) {
    stop("`max_subsites` must be a positive number or Inf.", call. = FALSE)
  }
  invisible(TRUE)
}

.cr_subsite_sex_code <- function(sex) {
  if (length(sex) != 1L || is.na(sex)) {
    stop("`sex` must be a single value.", call. = FALSE)
  }
  sex_chr <- tolower(as.character(sex))
  if (sex_chr %in% c("total", "both", "all", "0")) {
    return(c("1", "2"))
  }
  if (sex_chr %in% c("male", "men", "males", "1")) {
    return("1")
  }
  if (sex_chr %in% c("female", "women", "females", "2")) {
    return("2")
  }
  stop("`sex` must be one of 'total', 'male', 'female', 0, 1, or 2.", call. = FALSE)
}

.cr_pick_subsite_rows <- function(data, cancer, area, sex) {
  cancer_values <- .cr_subsite_cancer_values(cancer)
  area_values <- as.character(area)
  sex_values <- .cr_subsite_sex_code(sex)

  rows <- data[
    as.character(data$sex) %in% sex_values &
      (
        as.character(data$cancer) %in% cancer_values |
          as.character(data$cancer_cn) %in% cancer_values |
          as.character(data$cancer_en) %in% cancer_values
      ) &
      (
        as.character(data$areacode) %in% area_values |
          as.character(data$areacode_cn) %in% area_values |
          as.character(data$areacode_en) %in% area_values
      ),
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    stop("No ICD-10 subsite rows found for the requested cancer, area, and sex.", call. = FALSE)
  }
  rows
}

.cr_subsite_cancer_values <- function(cancer) {
  x <- as.character(cancer)
  zh_suffix <- "\u764c"
  en_suffix <- " cancer"

  unique(c(
    x,
    paste0(x, zh_suffix),
    sub(paste0(zh_suffix, "$"), "", x),
    paste0(x, en_suffix),
    sub(paste0(en_suffix, "$"), "", x, ignore.case = TRUE)
  ))
}

.cr_subsite_summary <- function(rows,
                                label_suffix,
                                unspecified_pattern,
                                digits,
                                max_subsites,
                                lang,
                                lang_data) {
  if (is.null(unspecified_pattern)) {
    unspecified_pattern <- "\u672a\u7279\u6307|unspecified"
  }

  rows$.subsite_name <- .cr_subsite_name(rows, label_suffix = label_suffix, lang = lang)
  rows$.is_complete <- !is.na(rows$.subsite_name) &
    !grepl(unspecified_pattern, paste(rows$icd10_cn, rows$icd10_en), ignore.case = TRUE)

  total_cases <- sum(rows$count, na.rm = TRUE)
  complete_rows <- rows[rows$.is_complete, , drop = FALSE]
  known_cases <- sum(complete_rows$count, na.rm = TRUE)
  if (total_cases <= 0 || known_cases <= 0 || nrow(complete_rows) == 0L) {
    stop("No complete subsite cases found after excluding unspecified subsites.", call. = FALSE)
  }

  subsite_counts <- stats::aggregate(
    count ~ .subsite_name,
    data = complete_rows[, c(".subsite_name", "count"), drop = FALSE],
    FUN = sum
  )
  subsite_counts <- subsite_counts[order(-subsite_counts$count, subsite_counts$.subsite_name), , drop = FALSE]
  if (is.finite(max_subsites)) {
    subsite_counts <- utils::head(subsite_counts, as.integer(max_subsites))
  }
  subsite_counts$prop <- subsite_counts$count / known_cases * 100
  subsite_names <- .cr_subsite_display_names(subsite_counts$.subsite_name, lang = lang)

  props <- paste0(.cr_format_vector(subsite_counts$prop, digits = digits), "%")
  list(
    n_subsites = nrow(subsite_counts),
    known_prop = .cr_format_number(known_cases / total_cases * 100, digits = digits),
    top_subsite = subsite_names[[1L]],
    top_prop = .cr_format_number(subsite_counts$prop[[1L]], digits = digits),
    other_subsites = .cr_subsite_collapse(subsite_names[-1L], lang = lang, lang_data = lang_data),
    other_props = .cr_subsite_collapse(props[-1L], lang = lang, lang_data = lang_data)
  )
}

.cr_subsite_display_names <- function(x, lang) {
  x <- as.character(x)
  if (identical(lang, "en")) {
    paste("the", x)
  } else {
    x
  }
}

.cr_subsite_name <- function(rows, label_suffix, lang) {
  x <- .cr_label_value(rows, "icd10", label_suffix)
  if (identical(lang, "zh")) {
    x <- sub("^\u80c3\u8d32\u95e8$", "\u8d32\u95e8", x)
    x <- sub("^\u80c3\u4ea4\u642d\u8de8\u8d8a$", "\u4ea4\u642d\u8de8\u8d8a", x)
    x <- sub("^\u6076\u6027\u80bf\u7624", "", x)
  } else {
    x <- sub(" of stomach$", "", x, ignore.case = TRUE)
    x <- sub(" lesion of stomach$", "", x, ignore.case = TRUE)
    x <- sub(", unspecified$", "", x, ignore.case = TRUE)
    x <- tolower(x)
  }
  x
}

.cr_subsite_collapse <- function(x, lang, lang_data) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return("")
  }
  if (identical(lang, "en")) {
    .cr_collapse_en(x, lang_data)
  } else {
    .cr_collapse_zh(x, lang_data)
  }
}
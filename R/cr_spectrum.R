#' Describe the leading cancers in a cancer spectrum
#'
#' `cr_spectrum()` creates a paragraph describing the top-ranked cancers for one
#' registry area and event type. By default, it reports the total, male, and
#' female cancer spectra; use `sex` to control which sex strata are included.
#'
#' @param data A data frame with ASR columns such as `areacode`, `areacode_cn`,
#'   `areacode_en`, `year`, `sex`, `cancer`, `cancer_cn`, `cancer_en`,
#'   `no_cases`, `prop`, and `rank`.
#' @param event_type Event type. One of `"incidence"` or `"mortality"`.
#' @param area Area code, Chinese area label, or English area label.
#' @param sex Sex strata to include. Defaults to `c(0, 1, 2)`, reporting total,
#'   male, and female spectra. Values may be numeric codes `0`, `1`, `2` or
#'   labels such as `"total"`, `"male"`, and `"female"`.
#' @param n Number of leading cancers used to calculate the cumulative
#'   proportion. Defaults to `10`.
#' @param top_n_names Number of leading cancer names to list in text. Defaults to
#'   `5`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param digits Number of decimal places for percentages.
#'
#' @return A character string.
#' @export
cr_spectrum <- function(data,
                        event_type = c("incidence", "mortality"),
                        area,
                        sex = c(0, 1, 2),
                        n = 10,
                        top_n_names = 5,
                        lang = "zh",
                        digits = 2) {
  event_type <- match.arg(event_type)
  lang <- match.arg(lang, c("zh", "en"))
  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }

  .cr_validate_spectrum_data(data)
  .cr_validate_spectrum_args(n = n, top_n_names = top_n_names, digits = digits)
  sex <- .cr_spectrum_sex_codes(sex)

  template <- .cr_load_template("spectrum", lang = lang)
  lang_data <- .cr_load_lang(lang)
  zh_lang_data <- .cr_load_lang("zh")
  en_lang_data <- .cr_load_lang("en")
  event_terms <- .cr_get_event_terms(event_type, lang = lang)
  label_suffix <- .cr_label_suffix(lang)

  available <- intersect(c("0", "1", "2"), as.character(sex))
  segments <- lapply(available, function(sex_code) {
    rows <- .cr_spectrum_rows(
      data,
      area = area,
      sex = sex_code,
      zh_pattern = zh_lang_data$spectrum$exclude_cancer_pattern,
      en_pattern = en_lang_data$spectrum$exclude_cancer_pattern
    )
    values <- .cr_spectrum_values(
      rows,
      sex_code = sex_code,
      n = n,
      top_n_names = top_n_names,
      label_suffix = label_suffix,
      event_terms = event_terms,
      lang_data = lang_data,
      lang = lang,
      digits = digits
    )
    .cr_render_text(template$overall[[.cr_spectrum_template_key(sex_code)]], values)
  })

  .cr_spectrum_join_segments(unlist(segments, use.names = FALSE), lang_data = lang_data)
}

.cr_validate_spectrum_data <- function(data) {
  required <- c(
    "areacode", "areacode_en", "areacode_cn", "year", "sex", "cancer",
    "cancer_en", "cancer_cn", "no_cases", "prop", "rank"
  )
  .cr_check_required_columns(data, required)
  if (!is.numeric(data$prop)) {
    stop("`prop` must be numeric.", call. = FALSE)
  }
  if (!is.numeric(data$rank)) {
    stop("`rank` must be numeric.", call. = FALSE)
  }
  invisible(data)
}

.cr_validate_spectrum_args <- function(n, top_n_names, digits) {
  for (arg in c("n", "top_n_names")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 1L || value != as.integer(value)) {
      stop("`", arg, "` must be a positive integer.", call. = FALSE)
    }
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0L) {
    stop("`digits` must be a non-negative number.", call. = FALSE)
  }
  invisible(TRUE)
}

.cr_spectrum_sex_codes <- function(sex) {
  if (length(sex) == 0L || any(is.na(sex))) {
    stop("`sex` must contain at least one non-missing value.", call. = FALSE)
  }
  sex_chr <- tolower(as.character(sex))
  out <- character(length(sex_chr))
  out[sex_chr %in% c("0", "total", "both", "all")] <- "0"
  out[sex_chr %in% c("1", "male", "men", "males")] <- "1"
  out[sex_chr %in% c("2", "female", "women", "females")] <- "2"
  if (any(out == "")) {
    stop("`sex` must use 0, 1, 2, 'total', 'male', or 'female'.", call. = FALSE)
  }
  intersect(c("0", "1", "2"), unique(out))
}

.cr_spectrum_rows <- function(data, area, sex, zh_pattern, en_pattern) {
  area_values <- as.character(area)
  rows <- data[
    as.character(data$sex) == as.character(sex) &
      (
        as.character(data$areacode) %in% area_values |
          as.character(data$areacode_cn) %in% area_values |
          as.character(data$areacode_en) %in% area_values
      ),
    ,
    drop = FALSE
  ]
  rows <- rows[.cr_spectrum_is_specific_cancer(rows, zh_pattern = zh_pattern, en_pattern = en_pattern), , drop = FALSE]
  rows <- rows[!is.na(rows$rank), , drop = FALSE]
  rows <- rows[order(rows$rank, -rows$no_cases, as.character(rows$cancer)), , drop = FALSE]

  if (nrow(rows) == 0L) {
    stop("No ranked cancer spectrum rows found for the requested area and sex.", call. = FALSE)
  }
  rows
}

.cr_spectrum_is_specific_cancer <- function(rows, zh_pattern, en_pattern) {
  cancer_values <- tolower(as.character(rows$cancer))
  cancer_cn <- as.character(rows$cancer_cn)
  cancer_en <- tolower(as.character(rows$cancer_en))
  !(cancer_values %in% c("0", "00", "000", "100", "all", "all cancers") |
      grepl(zh_pattern, cancer_cn) |
      grepl(en_pattern, cancer_en))
}

.cr_spectrum_values <- function(rows,
                                sex_code,
                                n,
                                top_n_names,
                                label_suffix,
                                event_terms,
                                lang_data,
                                lang,
                                digits) {
  name_rows <- utils::head(rows, top_n_names)
  top_n_rows <- utils::head(rows, n)
  cancer_names <- .cr_label_value(name_rows, "cancer", label_suffix)
  top_cancer <- cancer_names[[1L]]
  other_cancers <- cancer_names[-1L]
  if (length(other_cancers) == 0L) {
    other_cancers <- ""
  } else if (identical(lang, "en")) {
    other_cancers <- .cr_collapse_en(tolower(other_cancers), lang_data)
  } else {
    other_cancers <- .cr_collapse_zh(other_cancers, lang_data)
  }

  list(
    year = rows$year[[1L]],
    area_name = .cr_label_value(rows[1L, , drop = FALSE], "areacode", label_suffix),
    n = n,
    top_cancer = if (identical(lang, "en")) tolower(top_cancer) else top_cancer,
    top_cancer_sentence = .cr_sentence_case_en(top_cancer),
    other_cancers = other_cancers,
    top_prop = .cr_format_number(sum(top_n_rows$prop, na.rm = TRUE), digits = digits),
    event_noun = event_terms$noun,
    event_cause_label = .cr_spectrum_event_cause_label(event_terms),
    sex_label = .cr_spectrum_sex_label(sex_code, lang_data),
    phrase_registration_area = .cr_spectrum_registration_area_phrase(lang_data)
  )
}

.cr_spectrum_event_cause_label <- function(event_terms) {
  if (!is.null(event_terms$cause_label)) {
    return(event_terms$cause_label)
  }
  event_terms$noun
}

.cr_spectrum_registration_area_phrase <- function(lang_data) {
  if (!is.null(lang_data$phrase$registration_area)) {
    return(lang_data$phrase$registration_area)
  }
  ""
}

.cr_spectrum_template_key <- function(sex_code) {
  switch(as.character(sex_code), "0" = "total", "1" = "male", "2" = "female")
}

.cr_spectrum_sex_label <- function(sex_code, lang_data) {
  switch(as.character(sex_code),
    "0" = "",
    "1" = lang_data$sex$male,
    "2" = lang_data$sex$female
  )
}

.cr_spectrum_join_segments <- function(segments, lang_data) {
  segments <- segments[nzchar(segments)]
  if (length(segments) == 0L) return("")
  punctuation <- .cr_spectrum_punctuation(lang_data)
  if (isFALSE(lang_data$phrase$cancer_registration_area_prefix)) {
    return(.cr_spectrum_join_segments_zh(segments, punctuation = punctuation))
  }
  paste(segments, collapse = " ")
}

.cr_spectrum_join_segments_zh <- function(segments, punctuation) {
  if (length(segments) == 1L) {
    return(paste0(segments, punctuation$full_stop))
  }
  if (length(segments) == 2L) {
    return(paste0(segments[[1L]], punctuation$full_stop, segments[[2L]], punctuation$full_stop))
  }
  paste0(segments[[1L]], punctuation$full_stop, segments[[2L]], punctuation$semicolon, segments[[3L]], punctuation$full_stop)
}

.cr_spectrum_punctuation <- function(lang_data) {
  list(
    full_stop = lang_data$punctuation$full_stop,
    semicolon = lang_data$punctuation$semicolon
  )
}
# Internal utility helpers shared by canreport text generators.

#' Check that required columns exist
#'
#' Validates that an input data frame contains all columns required by a text
#' generator before downstream filtering or formatting is performed.
#'
#' @param data A data frame-like object.
#' @param required Character vector of required column names.
#' @param label Name used in the error message.
#'
#' @return `data`, invisibly, if all columns are present.
#' @noRd
.cr_check_required_columns <- function(data, required, label = "data") {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "`", label, "` is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(data)
}

#' Classify a two-value comparison
#'
#' Compares two numeric values and returns one of the supplied status labels,
#' optionally treating small relative differences as similar.
#'
#' @param left,right Numeric values to compare.
#' @param left_higher,right_higher Labels returned when the left or right value
#'   is higher.
#' @param similar Label returned when values are missing or within `threshold`.
#' @param threshold Relative-difference threshold for treating values as similar.
#'
#' @return A single status label.
#' @noRd
.cr_compare_status <- function(left,
                               right,
                               left_higher,
                               right_higher,
                               similar = "similar",
                               threshold = 0) {
  if (is.na(left) || is.na(right)) {
    return(similar)
  }

  denominator <- max(abs(left), abs(right), .Machine$double.eps)
  relative_diff <- abs(left - right) / denominator

  if (relative_diff <= threshold) {
    return(similar)
  }

  if (left > right) {
    left_higher
  } else {
    right_higher
  }
}

#' Calculate a larger-to-smaller ratio
#'
#' Computes `max(x, y) / min(x, y)` for subgroup comparisons. Missing or zero
#' values return `NA_real_` to avoid undefined ratios.
#'
#' @param x,y Numeric scalar values.
#'
#' @return A numeric scalar ratio, or `NA_real_`.
#' @noRd
.cr_ratio <- function(x, y) {
  if (is.na(x) || is.na(y) || x == 0 || y == 0) {
    return(NA_real_)
  }

  max(x, y) / min(x, y)
}

#' Get the language-specific label suffix
#'
#' Maps a report language code to the suffix used by labelled data columns.
#'
#' @param lang Language code, currently `"zh"` or `"en"`.
#'
#' @return `"cn"` for Chinese and `"en"` for English.
#' @noRd
.cr_label_suffix <- function(lang) {
  if (identical(lang, "en")) {
    "en"
  } else {
    "cn"
  }
}

#' Extract a language-specific label column
#'
#' Builds a column name from `stem` and `suffix` (for example,
#' `"cancer_en"`) and returns its character values.
#'
#' @param row A data frame containing labelled columns.
#' @param stem Column stem, such as `"cancer"`, `"areacode"`, or `"icd10"`.
#' @param suffix Language suffix, usually `"cn"` or `"en"`.
#'
#' @return A character vector with the selected label values.
#' @noRd
.cr_label_value <- function(row, stem, suffix) {
  column <- paste0(stem, "_", suffix)
  if (!column %in% names(row)) {
    stop("Column `", column, "` is required for this language.", call. = FALSE)
  }

  as.character(row[[column]])
}

#' Convert English text to sentence case
#'
#' Lowercases a single English label and capitalizes its first character. This
#' is used when a cancer name needs to start a sentence.
#'
#' @param x A single character value.
#'
#' @return A single character value, or `NA_character_` for invalid input.
#' @noRd
.cr_sentence_case_en <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }

  x <- tolower(as.character(x))
  paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
}

#' Format an English ordinal number
#'
#' Converts an integer rank to an English ordinal string, such as `"1st"`,
#' `"2nd"`, or `"11th"`.
#'
#' @param x A single integer-like value.
#'
#' @return A single character ordinal, or `NA_character_` for invalid input.
#' @noRd
.cr_ordinal_en <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }

  x <- as.integer(x)
  suffix <- if (x %% 100L %in% 11L:13L) {
    "th"
  } else {
    switch(
      as.character(x %% 10L),
      "1" = "st",
      "2" = "nd",
      "3" = "rd",
      "th"
    )
  }

  paste0(x, suffix)
}

#' Format a numeric scalar for report text
#'
#' Rounds and formats one numeric value with fixed decimal places and without
#' scientific notation.
#'
#' @param x A single numeric value.
#' @param digits Number of decimal places.
#'
#' @return A single formatted character value, or `NA_character_`.
#' @noRd
.cr_format_number <- function(x, digits = 2) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }

  if (digits == 0) {
    return(format(round(x), big.mark = "", scientific = FALSE, trim = TRUE))
  }

  format(
    round(x, digits),
    nsmall = digits,
    big.mark = "",
    scientific = FALSE,
    trim = TRUE
  )
}

#' Format a numeric vector for report text
#'
#' Vectorized wrapper around `.cr_format_number()`.
#'
#' @param x Numeric vector.
#' @param digits Number of decimal places.
#'
#' @return A character vector of formatted values.
#' @noRd
.cr_format_vector <- function(x, digits = 2) {
  vapply(x, .cr_format_number, character(1L), digits = digits)
}

#' Collapse a Chinese list
#'
#' Joins character values with Chinese list separators defined in the language
#' dictionary.
#'
#' @param x Character vector to collapse.
#' @param lang_data Parsed language dictionary.
#'
#' @return A single character string.
#' @noRd
.cr_collapse_zh <- function(x, lang_data) {
  x <- as.character(x)
  len <- length(x)
  if (len == 0L) return("")
  if (len == 1L) return(x)
  if (len == 2L) return(paste(x, collapse = lang_data$list$and))
  paste0(paste(x[-len], collapse = lang_data$list$comma), lang_data$list$and, x[len])
}

#' Collapse an English list
#'
#' Joins character values with English list separators defined in the language
#' dictionary.
#'
#' @param x Character vector to collapse.
#' @param lang_data Parsed language dictionary.
#'
#' @return A single character string.
#' @noRd
.cr_collapse_en <- function(x, lang_data) {
  x <- as.character(x)
  len <- length(x)
  if (len == 0L) return("")
  if (len == 1L) return(x)
  if (len == 2L) return(paste(x, collapse = paste0(" ", lang_data$list$and, " ")))
  paste0(paste(x[-len], collapse = lang_data$list$comma), " ", lang_data$list$and, " ", x[len])
}

#' Check a non-negative numeric formatting argument
#'
#' @param x Numeric scalar to check.
#' @param name Argument name used in the error message.
#'
#' @return `x`, invisibly, if valid.
#' @noRd
.cr_check_digits <- function(x, name = "digits") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0) {
    stop("`", name, "` must be a non-negative number.", call. = FALSE)
  }

  invisible(x)
}

#' Check a unit-interval threshold argument
#'
#' @param x Numeric scalar to check.
#' @param name Argument name used in the error message.
#'
#' @return `x`, invisibly, if valid.
#' @noRd
.cr_check_threshold <- function(x, name = "compare_threshold") {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || x > 1) {
    stop("`", name, "` must be a number between 0 and 1.", call. = FALSE)
  }

  invisible(x)
}

#' Match a text-generator output mode
#'
#' @param output Requested output mode.
#' @param choices Supported output modes.
#'
#' @return A single matched output mode.
#' @noRd
.cr_check_output <- function(output, choices = c("text", "data", "both")) {
  match.arg(output, choices)
}

#' Convert template values to a one-row data frame
#'
#' @param values Named list of scalar template values.
#'
#' @return A one-row data frame.
#' @noRd
.cr_values_data <- function(values) {
  values <- lapply(values, function(x) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }
    if (length(x) > 1L) {
      return(paste(as.character(x), collapse = ", "))
    }
    x
  })

  data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Return text, structured data, or both
#'
#' @param text Rendered report text.
#' @param values Named list of template values.
#' @param output Output mode, one of `"text"`, `"data"`, or `"both"`.
#'
#' @return A character string, a data frame, or a list.
#' @noRd
.cr_output_result <- function(text, values, output = c("text", "data", "both")) {
  output <- .cr_check_output(output)

  if (identical(output, "text")) {
    return(text)
  }

  data <- .cr_values_data(values)
  if (identical(output, "data")) {
    return(data)
  }

  list(
    text = text,
    data = data,
    values = values
  )
}
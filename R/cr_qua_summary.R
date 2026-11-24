#' Generate a quality indicator summary for cancer registration areas
#'
#' `cr_qua_summary()` describes key cancer registry quality indicators from a
#' `cr_quality`-style table, including MV%, M:I, DCO%, unknown site, and unknown
#' subsite indicators. It also reports sex-specific core indicators and compares
#' urban and rural MV% and M:I values.
#'
#' @param data A data frame with quality indicator columns such as `areacode`,
#'   `areacode_cn`, `areacode_en`, `year`, `sex`, `cancer`, `cancer_cn`,
#'   `cancer_en`, `mi`, `mv`, `dco`, `ub`, and `sub`.
#' @param area Main registration area code or label used for total, male, and
#'   female rows.
#' @param cancer Cancer code or label. Defaults to `60`, the all-cancers row in
#'   the example `cr_quality` dataset.
#' @param area_roles A named list identifying urban and rural rows. Defaults to
#'   `list(urban = 910000, rural = 920000)`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param digits Number of decimal places for percentages and ratios.
#' @param compare_threshold Relative threshold for treating urban and rural
#'   indicator values as similar.
#' @param output Output mode. `"text"` returns the rendered paragraph, `"data"`
#'   returns a one-row data frame of template values, and `"both"` returns both.
#'
#' @return A character string, data frame, or list, depending on `output`.
#' @export
cr_qua_summary <- function(data,
                           area,
                           cancer = 60,
                           area_roles = list(urban = 910000, rural = 920000),
                           lang = "zh",
                           digits = 2,
                           compare_threshold = 0,
                           output = c("text", "data", "both")) {
  lang <- match.arg(lang, c("zh", "en"))
  output <- .cr_check_output(output)
  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }
  .cr_validate_qua_summary_data(data)
  .cr_validate_qua_summary_args(digits = digits, compare_threshold = compare_threshold)

  template <- .cr_load_template("qua_summary", lang = lang)
  label_suffix <- .cr_label_suffix(lang)

  main_total <- .cr_pick_qua_summary_row(data, area = area, cancer = cancer, sex = 0)
  main_male <- .cr_pick_qua_summary_row(data, area = area, cancer = cancer, sex = 1)
  main_female <- .cr_pick_qua_summary_row(data, area = area, cancer = cancer, sex = 2)
  urban_total <- .cr_pick_qua_summary_row(data, area = area_roles$urban, cancer = cancer, sex = 0)
  rural_total <- .cr_pick_qua_summary_row(data, area = area_roles$rural, cancer = cancer, sex = 0)

  values <- list(
    year = main_total$year,
    area_name = .cr_label_value(main_total, "areacode", label_suffix),
    cancer_name = .cr_label_value(main_total, "cancer", label_suffix),
    cancer_name_lower = tolower(.cr_label_value(main_total, "cancer", label_suffix)),
    total_mv = .cr_format_number(main_total$mv, digits = digits),
    total_mi = .cr_format_number(main_total$mi, digits = digits),
    total_dco = .cr_format_number(main_total$dco, digits = digits),
    total_ub = .cr_format_number(main_total$ub, digits = digits),
    total_sub = .cr_format_number(main_total$sub, digits = digits),
    male_mv = .cr_format_number(main_male$mv, digits = digits),
    male_mi = .cr_format_number(main_male$mi, digits = digits),
    male_dco = .cr_format_number(main_male$dco, digits = digits),
    female_mv = .cr_format_number(main_female$mv, digits = digits),
    female_mi = .cr_format_number(main_female$mi, digits = digits),
    female_dco = .cr_format_number(main_female$dco, digits = digits),
    urban_mv = .cr_format_number(urban_total$mv, digits = digits),
    urban_mi = .cr_format_number(urban_total$mi, digits = digits),
    urban_dco = .cr_format_number(urban_total$dco, digits = digits),
    rural_mv = .cr_format_number(rural_total$mv, digits = digits),
    rural_mi = .cr_format_number(rural_total$mi, digits = digits),
    rural_dco = .cr_format_number(rural_total$dco, digits = digits)
  )

  mv_key <- .cr_compare_status(urban_total$mv, rural_total$mv, "urban_higher", "rural_higher", threshold = compare_threshold)
  mi_key <- .cr_compare_status(urban_total$mi, rural_total$mi, "urban_higher", "rural_higher", threshold = compare_threshold)
  values$mv_compare_text <- .cr_render_text(template$comparison$mv[[mv_key]], values)
  values$mi_compare_text <- .cr_render_text(template$comparison$mi[[mi_key]], values)

  text <- .cr_render_text(template$overall$default, values)
  .cr_output_result(text, values, output = output)
}

.cr_validate_qua_summary_data <- function(data) {
  required <- c(
    "areacode", "areacode_en", "areacode_cn", "year", "sex",
    "cancer", "cancer_en", "cancer_cn", "mi", "mv", "dco", "ub", "sub"
  )
  .cr_check_required_columns(data, required)
  numeric_columns <- c("mi", "mv", "dco", "ub", "sub")
  for (column in numeric_columns) {
    if (!is.numeric(data[[column]])) {
      stop("`", column, "` must be numeric.", call. = FALSE)
    }
  }
  invisible(data)
}

.cr_validate_qua_summary_args <- function(digits, compare_threshold) {
  .cr_check_digits(digits)
  .cr_check_threshold(compare_threshold)
  invisible(TRUE)
}

.cr_pick_qua_summary_row <- function(data, area, cancer, sex) {
  area_values <- as.character(area)
  cancer_values <- as.character(cancer)
  row <- data[
    as.character(data$sex) == as.character(sex) &
      (
        as.character(data$areacode) %in% area_values |
          as.character(data$areacode_cn) %in% area_values |
          as.character(data$areacode_en) %in% area_values
      ) &
      (
        as.character(data$cancer) %in% cancer_values |
          as.character(data$cancer_cn) %in% cancer_values |
          as.character(data$cancer_en) %in% cancer_values
      ),
    ,
    drop = FALSE
  ]
  if (nrow(row) == 0L) {
    stop("No quality indicator row found for the requested area, cancer, and sex.", call. = FALSE)
  }
  if (nrow(row) > 1L) {
    stop("Multiple quality indicator rows found for the requested area, cancer, and sex.", call. = FALSE)
  }
  row
}
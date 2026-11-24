#' Generate a population coverage paragraph for cancer registration areas
#'
#' `cr_coverage()` describes the population covered by cancer registration areas
#' included in an annual report analysis. It uses the population counts in a
#' `cr_pop`-style table and an externally supplied provincial population total
#' to calculate the registration-area coverage proportion.
#'
#' @param data A data frame with population columns such as `areacode`,
#'   `areacode_cn`, `areacode_en`, `year`, `sex`, and `rks`.
#' @param area Main registration area code or label used for the total covered
#'   population and sex-specific covered populations.
#' @param pop_total Externally supplied total provincial population.
#' @param report_year Annual report year. If `NULL`, the data year is used.
#' @param pop_source Optional source text for the provincial population total.
#' @param area_roles A named list identifying urban and rural rows. Defaults to
#'   `list(urban = 910000, rural = 920000)`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param digits Number of decimal places for percentages.
#'
#' @return A character string.
#' @export
cr_coverage <- function(data,
                        area,
                        pop_total,
                        report_year = NULL,
                        pop_source = NULL,
                        area_roles = list(urban = 910000, rural = 920000),
                        lang = "zh",
                        digits = 2) {
  lang <- match.arg(lang, c("zh", "en"))

  if (missing(area) || is.null(area)) {
    stop("`area` must be supplied.", call. = FALSE)
  }
  if (missing(pop_total) || is.null(pop_total)) {
    stop("`pop_total` must be supplied.", call. = FALSE)
  }
  if (!is.numeric(pop_total) || length(pop_total) != 1L || is.na(pop_total) || pop_total <= 0) {
    stop("`pop_total` must be a positive numeric scalar.", call. = FALSE)
  }

  .cr_validate_coverage_data(data)

  template <- .cr_load_template("coverage", lang = lang)
  lang_data <- .cr_load_lang(lang)
  label_suffix <- .cr_label_suffix(lang)

  data_year <- .cr_coverage_year(data)
  if (is.null(report_year)) {
    report_year <- data_year
  }

  main_rows <- .cr_pick_coverage_rows(data, area = area)
  male_rows <- main_rows[as.character(main_rows$sex) == "1", , drop = FALSE]
  female_rows <- main_rows[as.character(main_rows$sex) == "2", , drop = FALSE]
  urban_rows <- .cr_pick_coverage_rows(data, area = area_roles$urban)
  rural_rows <- .cr_pick_coverage_rows(data, area = area_roles$rural)

  covered_population <- .cr_sum_population(main_rows)
  male_population <- .cr_sum_population(male_rows)
  female_population <- .cr_sum_population(female_rows)
  urban_population <- .cr_sum_population(urban_rows)
  rural_population <- .cr_sum_population(rural_rows)

  values <- list(
    report_year = report_year,
    data_year = data_year,
    area_name = .cr_label_value(main_rows[1L, , drop = FALSE], "areacode", label_suffix),
    covered_population = .cr_format_integer_space(covered_population),
    male_population = .cr_format_integer_space(male_population),
    female_population = .cr_format_integer_space(female_population),
    pop_total = .cr_format_integer_space(pop_total),
    coverage_prop = .cr_format_number(covered_population / pop_total * 100, digits = digits),
    urban_population = .cr_format_integer_space(urban_population),
    rural_population = .cr_format_integer_space(rural_population),
    urban_prop = .cr_format_number(urban_population / covered_population * 100, digits = digits),
    rural_prop = .cr_format_number(rural_population / covered_population * 100, digits = digits),
    pop_source_text = .cr_coverage_source_text(pop_source, lang_data = lang_data)
  )

  .cr_render_text(template$overall$default, values)
}

.cr_validate_coverage_data <- function(data) {
  required <- c("areacode", "areacode_en", "areacode_cn", "year", "sex", "rks")
  .cr_check_required_columns(data, required)
  invisible(data)
}

.cr_pick_coverage_rows <- function(data, area) {
  area_values <- as.character(area)
  rows <- data[
    as.character(data$areacode) %in% area_values |
      as.character(data$areacode_cn) %in% area_values |
      as.character(data$areacode_en) %in% area_values,
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    stop("No population rows found for the requested area.", call. = FALSE)
  }

  rows
}

.cr_coverage_year <- function(data) {
  years <- unique(data$year)
  years <- years[!is.na(years)]
  if (length(years) != 1L) {
    stop("`data` must contain exactly one non-missing year.", call. = FALSE)
  }
  years[[1L]]
}

.cr_sum_population <- function(data) {
  sum(data$rks, na.rm = TRUE)
}

.cr_format_integer_space <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
}

.cr_coverage_source_text <- function(pop_source, lang_data) {
  if (is.null(pop_source) || length(pop_source) == 0L || is.na(pop_source) || identical(pop_source, "")) {
    return("")
  }
  paste0(
    lang_data$punctuation$paren_open,
    pop_source,
    lang_data$punctuation$paren_close
  )
}
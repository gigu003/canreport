#' Generate epidemiological indicator formula descriptions
#'
#' `cr_formula()` returns Markdown text describing common epidemiological and
#' cancer registry quality indicators, including their definitions, formulas,
#' symbol notes, and interpretation guidance. The output can be inserted into
#' Quarto or R Markdown reports with `cat()` and `results: asis`.
#'
#' @param indicator Indicator key or keys. Available indicators include
#'   `"average_population"`, `"crude_rate"`, `"age_specific_rate"`,
#'   `"age_standardized_rate"`, `"proportion"`, `"cumulative_rate"`,
#'   `"truncated_rate"`, `"mi_ratio"`, `"mv_percent"`, `"dco_percent"`, and
#'   `"unknown_percent"`.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param heading_level Markdown heading level for indicator titles. Use `NULL`
#'   to omit headings.
#' @param include Sections to include. Use `"all"` for all sections, or any
#'   combination of `"title"`, `"description"`, `"equation"`, `"note"`, and
#'   `"interpretation"`.
#' @param label Logical. If `FALSE`, Quarto equation labels such as
#'   `{#eq-crude-rate}` are removed from equations.
#' @param collapse String used to join multiple indicator blocks.
#'
#' @return A single Markdown character string.
#' @export
#'
#' @examples
#' cr_formula("crude_rate", lang = "en")
#' cr_formula(c("crude_rate", "age_standardized_rate"), lang = "zh")
cr_formula <- function(indicator,
                       lang = "zh",
                       heading_level = 4,
                       include = "all",
                       label = TRUE,
                       collapse = "\n\n") {
  lang <- match.arg(lang, c("zh", "en"))
  .cr_validate_formula_args(indicator, heading_level, include, label, collapse)

  template <- .cr_load_template("formula", lang = lang)
  available <- names(template)
  unknown <- setdiff(indicator, available)
  if (length(unknown) > 0L) {
    stop(
      "Unknown formula indicator: ", paste(unknown, collapse = ", "),
      ". Available indicators are: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }

  include <- .cr_normalize_formula_include(include)
  blocks <- vapply(
    indicator,
    function(x) .cr_render_formula_block(template[[x]], heading_level, include, label),
    character(1L),
    USE.NAMES = FALSE
  )

  paste(blocks, collapse = collapse)
}

.cr_validate_formula_args <- function(indicator, heading_level, include, label, collapse) {
  if (!is.character(indicator) || length(indicator) == 0L || any(!nzchar(indicator))) {
    stop("`indicator` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.null(heading_level)) {
    if (!is.numeric(heading_level) || length(heading_level) != 1L || is.na(heading_level) ||
        heading_level < 1L || heading_level > 6L || heading_level != as.integer(heading_level)) {
      stop("`heading_level` must be an integer between 1 and 6, or NULL.", call. = FALSE)
    }
  }
  if (!is.character(include) || length(include) == 0L || any(!nzchar(include))) {
    stop("`include` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.logical(label) || length(label) != 1L || is.na(label)) {
    stop("`label` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(collapse) || length(collapse) != 1L || is.na(collapse)) {
    stop("`collapse` must be a single character string.", call. = FALSE)
  }

  invisible(TRUE)
}

.cr_normalize_formula_include <- function(include) {
  allowed <- c("title", "description", "equation", "note", "interpretation")
  if (any(include == "all")) {
    return(allowed)
  }

  unknown <- setdiff(include, allowed)
  if (length(unknown) > 0L) {
    stop(
      "Unknown `include` value: ", paste(unknown, collapse = ", "),
      ". Allowed values are: all, ", paste(allowed, collapse = ", "),
      call. = FALSE
    )
  }

  unique(include)
}

.cr_render_formula_block <- function(item, heading_level, include, label) {
  parts <- character(0L)

  if ("title" %in% include && !is.null(heading_level)) {
    parts <- c(parts, paste0(strrep("#", heading_level), " ", item$title))
  }
  if ("description" %in% include) {
    parts <- c(parts, item$description)
  }
  if ("equation" %in% include) {
    equation <- item$equation
    if (!label) {
      equation <- .cr_strip_quarto_equation_label(equation)
    }
    parts <- c(parts, equation)
  }
  if ("note" %in% include) {
    parts <- c(parts, item$note)
  }
  if ("interpretation" %in% include) {
    parts <- c(parts, item$interpretation)
  }

  paste(trimws(parts), collapse = "\n\n")
}

.cr_strip_quarto_equation_label <- function(equation) {
  gsub("[[:space:]]*\\{#eq-[A-Za-z0-9_-]+\\}", "", equation, perl = TRUE)
}
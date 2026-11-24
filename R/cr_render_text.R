# Internal text rendering helper using {name} placeholders.

#' Render a text template with named values
#'
#' Replaces `{name}` placeholders in a single template string with values from a
#' named list or vector. The helper also normalizes whitespace and removes extra
#' spaces before common CJK punctuation.
#'
#' @param template A single character template containing `{name}` placeholders.
#' @param values A named list or vector providing replacement values.
#'
#' @return A single rendered character string.
#' @noRd
.cr_render_text <- function(template, values) {
  if (!is.character(template) || length(template) != 1L) {
    stop("`template` must be a single character string.", call. = FALSE)
  }
  if (is.null(names(values))) {
    stop("`values` must be a named list or vector.", call. = FALSE)
  }

  .cr_check_template_values(template, values)

  text <- template

  for (name in names(values)) {
    pattern <- paste0("{", name, "}")
    replacement <- values[[name]]
    if (is.null(replacement) || length(replacement) == 0L) {
      replacement <- ""
    }
    replacement <- as.character(replacement)
    if (length(replacement) == 0L || is.na(replacement)) {
      replacement <- ""
    }
    text <- gsub(pattern, replacement, text, fixed = TRUE)
  }

  text <- gsub("[[:space:]]+", " ", text)
  cjk_punct <- "([\uFF0C\u3002\uFF1B\uFF1A\u3001\uFF01\uFF1F]) "
  text <- gsub(cjk_punct, "\\1", text)
  trimws(text)
}

#' Extract placeholders from a template
#'
#' Finds placeholder names of the form `{name}` where `name` starts with a
#' letter and contains letters, digits, or underscores.
#'
#' @param template A single character template.
#'
#' @return A character vector of unique placeholder names.
#' @noRd
.cr_template_placeholders <- function(template) {
  matches <- gregexpr("\\{[A-Za-z][A-Za-z0-9_]*\\}", template, perl = TRUE)
  placeholders <- regmatches(template, matches)[[1L]]
  if (length(placeholders) == 0L || identical(placeholders, character(0L))) {
    return(character(0L))
  }
  unique(sub("^\\{(.+)\\}$", "\\1", placeholders))
}

#' Check that all template placeholders have values
#'
#' Validates that every placeholder in `template` is present in `names(values)`
#' before rendering text.
#'
#' @param template A single character template.
#' @param values A named list or vector of replacement values.
#'
#' @return `TRUE`, invisibly, if all placeholders are provided.
#' @noRd
.cr_check_template_values <- function(template, values) {
  placeholders <- .cr_template_placeholders(template)
  missing <- setdiff(placeholders, names(values))
  if (length(missing) > 0L) {
    stop(
      "Template values are missing required placeholders: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

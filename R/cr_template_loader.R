# Internal helpers for locating and reading template resources.

#' Resolve an installed or local package resource path
#'
#' Locates resources used by canreport both during package development, where
#' files live under `inst/`, and after installation, where `system.file()` omits
#' the leading `inst` component.
#'
#' @param ... Path components for the resource.
#'
#' @return A single existing file path.
#' @noRd
.cr_resource_path <- function(...) {
  parts <- c(...)
  installed_parts <- if (length(parts) > 0L && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }

  local_path <- do.call(file.path, as.list(parts))
  if (file.exists(local_path)) {
    return(local_path)
  }

  path <- do.call(system.file, c(as.list(installed_parts), list(package = "canreport")))

  if (!identical(path, "") && file.exists(path)) {
    return(path)
  }

  stop(
    "Cannot find canreport resource: ",
    file.path(...),
    call. = FALSE
  )
}

#' Load a report text template
#'
#' Reads a YAML template from `inst/templates/{lang}/{name}.yml`.
#'
#' @param name Template name without file extension.
#' @param lang Language code, currently `"zh"` or `"en"`.
#'
#' @return A list parsed from the YAML template.
#' @noRd
.cr_load_template <- function(name, lang = "zh") {
  path <- .cr_resource_path("inst", "templates", lang, paste0(name, ".yml"))
  yaml::read_yaml(path)
}

#' Load a language dictionary
#'
#' Reads common language resources from `inst/lang/{lang}.yml`.
#'
#' @param lang Language code, currently `"zh"` or `"en"`.
#'
#' @return A list parsed from the YAML dictionary.
#' @noRd
.cr_load_lang <- function(lang = "zh") {
  path <- .cr_resource_path("inst", "lang", paste0(lang, ".yml"))
  yaml::read_yaml(path)
}

#' Get event-specific language terms
#'
#' Retrieves dictionary terms for incidence or mortality in a given language.
#'
#' @param event_type Event type, one of `"incidence"` or `"mortality"`.
#' @param lang Language code, currently `"zh"` or `"en"`.
#'
#' @return A named list of event-specific terms.
#' @noRd
.cr_get_event_terms <- function(event_type, lang = "zh") {
  event_type <- match.arg(event_type, c("incidence", "mortality"))
  lang_data <- .cr_load_lang(lang)
  terms <- lang_data$event_type[[event_type]]

  if (is.null(terms)) {
    stop("Unsupported event_type: ", event_type, call. = FALSE)
  }

  terms
}

#' Load a built-in Markdown report chapter
#'
#' Reads a Markdown chapter from `inst/chapters/{lang}/{name}.md`.
#'
#' @param name Chapter name without file extension.
#' @param lang Language code, currently `"zh"` or `"en"`.
#'
#' @return A single character string containing the Markdown source.
#' @noRd
.cr_load_chapter <- function(name, lang = "zh") {
  path <- .cr_resource_path("inst", "chapters", lang, paste0(name, ".md"))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

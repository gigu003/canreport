#' Load a built-in Markdown report chapter
#'
#' `cr_chapters()` returns the Markdown source of built-in report chapters such
#' as the foreword, summary, and methods chapters. The function does not execute
#' code chunks in the chapter. If `values` is supplied, simple `{name}`
#' placeholders are replaced using the same renderer as other canreport text
#' generators; otherwise the Markdown is returned unchanged.
#'
#' @param chapter Chapter name. Built-in chapters currently include
#'   `"forword"`, `"summary"`, and `"methods"`. The aliases `"foreword"` and
#'   `"forward"` are accepted for `"forword"` for convenience.
#' @param lang Language code. One of `"zh"` or `"en"`.
#' @param values Optional named list or vector used to replace simple `{name}`
#'   placeholders. The default `NULL` returns the Markdown source unchanged,
#'   which is useful for chapters containing Quarto, epoxy, mermaid, or R code
#'   chunks.
#'
#' @return A single character string containing a Markdown chapter.
#' @export
#'
#' @examples
#' cr_chapters("summary", lang = "en")
#' cr_chapters("methods", lang = "zh")
cr_chapters <- function(chapter,
                        lang = "zh",
                        values = NULL) {
  lang <- match.arg(lang, c("zh", "en"))
  .cr_validate_chapter_args(chapter, values)

  chapter <- .cr_normalize_chapter_name(chapter)
  text <- .cr_load_chapter(chapter, lang = lang)

  if (!is.null(values)) {
    text <- .cr_render_text(text, values)
  }

  text
}

#' @rdname cr_chapters
#' @export
cr_chapter <- cr_chapters

.cr_validate_chapter_args <- function(chapter, values) {
  if (!is.character(chapter) || length(chapter) != 1L || is.na(chapter) || !nzchar(chapter)) {
    stop("`chapter` must be a single non-empty character string.", call. = FALSE)
  }

  if (!is.null(values) && is.null(names(values))) {
    stop("`values` must be NULL or a named list/vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.cr_normalize_chapter_name <- function(chapter) {
  chapter <- tolower(trimws(chapter))

  aliases <- c(
    foreword = "forword",
    forward = "forword"
  )

  if (chapter %in% names(aliases)) {
    return(unname(aliases[[chapter]]))
  }

  chapter
}

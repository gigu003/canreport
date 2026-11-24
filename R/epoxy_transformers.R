#' Manuscript Epoxy Transformers List
#'
#' A list of custom transformer functions that can be used with `epoxy::epoxy_transform_set()`.
#' This is provided for optional integration with the epoxy package.
#'
#' @format A named list of functions.
#' @export
epoxy_transformers <- function() {
  en <- .cr_load_lang("en")
  zh <- .cr_load_lang("zh")

  list(
    .per = function(x) paste0(x, "%"),
    .percent = function(x) {
      paste0(formatC(x * 100, format = "f", digits = 2), "%")
    },
    .and_en = function(x) {
      glue::glue_collapse(x, sep = ", ", last = paste0(" ", en$list$and, " "))
    },
    .and_cn = function(x) {
      glue::glue_collapse(x, sep = ", ", last = zh$list$and)
    },
    .rate_en = function(x) {
      paste0(
        formatC(x, format = "f", digits = 2),
        paste0(" ", en$phrase$per_100000)
      )
    },
    .rate_cn = function(x) {
      paste0(
        formatC(x, format = "f", digits = 2),
        zh$phrase$per_100000
      )
    },
    .cancer_en = function(x) paste0(x, " cancer"),
    .cancer_cn = function(x) {
      ifelse(grepl(zh$spectrum$cancer_name_pattern, x), x, paste0(x, "\u764c"))
    },
    .digit2 = function(x) formatC(x, format = "f", digits = 2),
    .tolower = function(x) tolower(x),
    .space = function(x) {
      function(x) prettyNum(round(x), big.mark = " ")
    },
    .rank_en = function(x) paste0(x, "^th^"),
    .rank_cn = function(x) paste0("\u7b2c", x, "\u4f4d")
  )
}

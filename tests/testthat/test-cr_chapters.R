test_that("cr_chapters loads built-in Markdown chapters", {
  zh_summary <- cr_chapters("summary", lang = "zh")
  en_summary <- cr_chapters("summary", lang = "en")
  zh_methods <- cr_chapters("methods", lang = "zh")
  en_methods <- cr_chapters("methods", lang = "en")
  expect_match(zh_summary, "# 概述", fixed = TRUE)
  expect_match(en_summary, "# Summary", fixed = TRUE)
  expect_match(zh_methods, "# 方法和统计学指标", fixed = TRUE)
  expect_match(en_methods, "# Methods and statistical indicators", fixed = TRUE)
})

test_that("cr_chapters supports foreword aliases", {
  direct <- cr_chapters("forword", lang = "en")
  expect_equal(cr_chapters("foreword", lang = "en"), direct)
  expect_equal(cr_chapters("forward", lang = "en"), direct)
  expect_match(direct, "# Foreword", fixed = TRUE)
})

test_that("cr_chapters returns Markdown source unchanged by default", {
  text <- cr_chapters("forword", lang = "zh")
  expect_match(text, "```{epoxy}", fixed = TRUE)
  expect_match(text, "{year.pub}", fixed = TRUE)
})

test_that("cr_chapter alias is available", {
  expect_equal(cr_chapter("summary", lang = "zh"), cr_chapters("summary", lang = "zh"))
})

test_that("cr_chapters validates inputs and missing resources", {
  expect_error(cr_chapters(character(0)), "`chapter` must be")
  expect_error(cr_chapters("summary", values = list("x")), "`values` must")
  expect_error(cr_chapters("unknown_chapter"), "Cannot find canreport resource")
})

test_that("built-in chapters exist in both languages", {
  chapter_names <- c("forword", "summary", "methods")
  for (lang in c("zh", "en")) {
    for (name in chapter_names) {
      path <- .cr_resource_path("inst", "chapters", lang, paste0(name, ".md"))
      expect_true(file.exists(path))
      text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      expect_true(nzchar(trimws(text)))
    }
  }
})

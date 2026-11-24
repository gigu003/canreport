test_that("cr_formula generates Chinese and English formula text", {
  zh <- cr_formula("crude_rate", lang = "zh")
  en <- cr_formula("crude_rate", lang = "en")

  expect_match(zh, "粗发病率/粗死亡率", fixed = TRUE)
  expect_match(zh, "\\frac", fixed = TRUE)
  expect_match(zh, "{#eq-crude-rate}", fixed = TRUE)

  expect_match(en, "Crude incidence or mortality rate", fixed = TRUE)
  expect_match(en, "per 100,000 population", fixed = TRUE)
})

test_that("cr_formula supports multiple indicators", {
  text <- cr_formula(c("crude_rate", "age_standardized_rate"), lang = "en")

  expect_match(text, "Crude incidence or mortality rate", fixed = TRUE)
  expect_match(text, "Age-standardized rate", fixed = TRUE)
})

test_that("cr_formula can omit labels and headings", {
  text <- cr_formula(
    "age_standardized_rate",
    lang = "zh",
    heading_level = NULL,
    include = c("equation"),
    label = FALSE
  )

  expect_match(text, "ASR", fixed = TRUE)
  expect_false(grepl("#eq-age-standardized-rate", text, fixed = TRUE))
  expect_false(grepl("####", text, fixed = TRUE))
})

test_that("cr_formula validates indicator and include values", {
  expect_error(cr_formula("not_a_formula"), "Unknown formula indicator")
  expect_error(cr_formula("crude_rate", include = "bad_section"), "Unknown `include` value")
})

test_that("formula templates are available in both languages", {
  required <- c(
    "average_population",
    "crude_rate",
    "age_specific_rate",
    "age_standardized_rate",
    "proportion",
    "cumulative_rate",
    "truncated_rate",
    "mi_ratio",
    "mv_percent",
    "dco_percent",
    "unknown_percent"
  )

  for (lang in c("zh", "en")) {
    template <- .cr_load_template("formula", lang = lang)
    expect_true(all(required %in% names(template)))
    for (indicator in required) {
      expect_true(all(c("title", "label", "description", "equation", "note", "interpretation") %in% names(template[[indicator]])))
    }
  }
})
test_that("summary blocks interpolate data and bilingual labels", {
  data <- list(rank = 1, no_cases = 100, cr = 10, asr_cn2000 = 5)
  zh <- cr_summary_block(data)
  en <- cr_summary_block(data, type = "mortality", lang = "en")

  expect_match(zh, '"顺位", "新发病例", "粗发病率", "中标率"', fixed = TRUE)
  expect_match(zh, '"1", "100", "10", "5"', fixed = TRUE)
  expect_match(
    en,
    '"rank", "deaths", "crude mortality rate", "ASR China"',
    fixed = TRUE
  )
})

test_that("language-dependent epoxy transformers work without global objects", {
  transforms <- epoxy_transformers()

  expect_equal(
    as.character(transforms$.and_en(c("lung", "liver"))),
    "lung and liver"
  )
  expect_equal(as.character(transforms$.and_cn(c("肺", "肝"))), "肺和肝")
  expect_equal(transforms$.rate_en(10), "10.00 per 100,000 population")
  expect_equal(transforms$.rate_cn(10), "10.00/10万")
  expect_equal(transforms$.cancer_en("lung"), "lung cancer")
  expect_equal(transforms$.cancer_cn(c("肺", "白血病")), c("肺癌", "白血病"))
})

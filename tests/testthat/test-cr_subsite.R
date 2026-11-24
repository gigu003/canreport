data("site_icd10", package = "canreport")

test_that("cr_subsite describes stomach subsites in Chinese", {
  text <- cr_subsite(site_icd10, cancer = "胃", area = 410000, lang = "zh")
  text_with_suffix <- cr_subsite(site_icd10, cancer = "胃癌", area = 410000, lang = "zh")

  expect_match(text, "全部胃癌病例中，具有明确亚部位信息的病例占", fixed = TRUE)
  expect_match(text, "明确亚部位信息", fixed = TRUE)
  expect_match(text, "胃癌病例", fixed = TRUE)
  expect_match(text, "贲门", fixed = TRUE)
  expect_match(text, "65\\.4[0-9]%")
  expect_match(text, "49\\.3[0-9]%")
  expect_identical(text, text_with_suffix)
})

test_that("cr_subsite describes stomach subsites in English", {
  text <- cr_subsite(site_icd10, cancer = "Stomach", area = 410000, lang = "en")
  text_with_suffix <- cr_subsite(site_icd10, cancer = "Stomach cancer", area = 410000, lang = "en")

  expect_match(text, "complete subsite information", fixed = TRUE)
  expect_match(text, "stomach cancer cases", fixed = TRUE)
  expect_false(grepl("the stomach cancer cases", text, fixed = TRUE))
  expect_false(grepl("cancer cancer cases", text, fixed = TRUE))
  expect_match(text, "Among these cases, the cardia was the most common subsite", fixed = TRUE)
  expect_match(text, "accounting for 49", fixed = TRUE)
  expect_match(text, "followed by the body, the fundus", fixed = TRUE)
  expect_false(grepl("complete information on subsite", text, fixed = TRUE))
  expect_false(grepl("with the proportion of", text, fixed = TRUE))
  expect_false(grepl("Cardia was", text, fixed = TRUE))
  expect_false(grepl("followed by Body", text, fixed = TRUE))
  expect_match(text, "65\\.4[0-9]%")
  expect_match(text, "49\\.3[0-9]%")
  expect_identical(text, text_with_suffix)
})
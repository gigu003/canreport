make_spectrum_data <- function() {
  cancers <- data.frame(
    cancer = as.character(101:110),
    cancer_cn = c("肺癌", "乳腺癌", "胃癌", "肝癌", "食管癌", "结直肠癌", "甲状腺癌", "胰腺癌", "子宫颈癌", "卵巢癌"),
    cancer_en = c("Lung cancer", "Breast cancer", "Stomach cancer", "Liver cancer", "Esophagus cancer", "Colorectal cancer", "Thyroid cancer", "Pancreas cancer", "Cervix Uteri cancer", "Ovary cancer"),
    stringsAsFactors = FALSE
  )
  total_order <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  male_order <- c(1, 3, 4, 5, 6, 8, 7, 9, 10, 2)
  female_order <- c(2, 1, 7, 3, 6, 9, 10, 8, 4, 5)
  props <- list(
    total = c(20, 15, 12, 10, 8, 5, 4, 3, 1.5, 0.76),
    male = c(24, 16, 14, 12, 8, 4, 3, 2, 0.7, 0.37),
    female = c(22, 18, 15, 12, 8, 3, 2, 1, 0.7, 0.27)
  )
  orders <- list(total_order, male_order, female_order)
  names(orders) <- c("total", "male", "female")

  rows <- lapply(seq_along(orders), function(i) {
    sex_code <- i - 1L
    ord <- orders[[i]]
    x <- cancers[ord, , drop = FALSE]
    x$rank <- seq_len(nrow(x))
    x$prop <- props[[i]]
    x$no_cases <- rev(seq_len(nrow(x))) * 100
    x$areacode <- 410000
    x$areacode_cn <- "河南省"
    x$areacode_en <- "Henan Province"
    x$year <- 2022
    x$sex <- sex_code
    x$sex_cn <- c("合计", "男性", "女性")[[i]]
    x$sex_en <- c("Total", "Male", "Female")[[i]]
    x
  })

  do.call(rbind, rows)
}

test_that("cr_spectrum renders Chinese total, male, and female cancer spectrum", {
  out <- cr_spectrum(make_spectrum_data(), event_type = "incidence", area = 410000, lang = "zh")

  expect_match(out, "2022年，河南省肿瘤登记地区恶性肿瘤发病第1位是肺癌，其次为乳腺癌，胃癌，肝癌和食管癌", fixed = TRUE)
  expect_match(out, "前10位恶性肿瘤发病人数占全部恶性肿瘤的79.26%", fixed = TRUE)
  expect_match(out, "男性恶性肿瘤发病第1位的是肺癌，其次为胃癌，肝癌，食管癌和结直肠癌", fixed = TRUE)
  expect_match(out, "男性前10位恶性肿瘤发病人数占全部恶性肿瘤的84.07%", fixed = TRUE)
  expect_match(out, "女性恶性肿瘤发病第1位的是乳腺癌，其次为肺癌，甲状腺癌，胃癌和结直肠癌", fixed = TRUE)
  expect_match(out, "女性前10位恶性肿瘤发病人数占全部恶性肿瘤的81.97%", fixed = TRUE)
})

test_that("cr_spectrum sex controls which strata are rendered", {
  out_total <- cr_spectrum(make_spectrum_data(), area = 410000, sex = 0, lang = "zh")
  out_sex <- cr_spectrum(make_spectrum_data(), area = 410000, sex = c("male", "female"), lang = "zh")

  expect_match(out_total, "2022年，河南省", fixed = TRUE)
  expect_false(grepl("男性恶性肿瘤", out_total, fixed = TRUE))
  expect_false(grepl("女性恶性肿瘤", out_total, fixed = TRUE))
  expect_false(grepl("2022年，河南省", out_sex, fixed = TRUE))
  expect_match(out_sex, "男性恶性肿瘤", fixed = TRUE)
  expect_match(out_sex, "女性恶性肿瘤", fixed = TRUE)
})

test_that("cr_spectrum renders English and mortality terms", {
  out <- cr_spectrum(
    make_spectrum_data(),
    event_type = "mortality",
    area = "Henan Province",
    sex = c(0, 1),
    lang = "en"
  )

  expect_match(out, "In 2022, lung cancer ranked first for cancer mortality", fixed = TRUE)
  expect_match(out, "followed by breast cancer, stomach cancer, liver cancer and esophagus cancer", fixed = TRUE)
  expect_match(out, "all cancer deaths", fixed = TRUE)
  expect_match(out, "Among males, lung cancer ranked first", fixed = TRUE)
  expect_false(grepl("Among females", out, fixed = TRUE))
})

test_that("cr_spectrum supports n and top_n_names separately", {
  out <- cr_spectrum(make_spectrum_data(), area = 410000, sex = 0, n = 3, top_n_names = 3, lang = "zh")

  expect_match(out, "其次为乳腺癌和胃癌", fixed = TRUE)
  expect_match(out, "前3位恶性肿瘤发病人数占全部恶性肿瘤的47.00%", fixed = TRUE)
})

test_that("cr_spectrum validates inputs", {
  dat <- make_spectrum_data()
  expect_error(cr_spectrum(dat), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_spectrum(dat, area = 410000, sex = 3), "`sex` must use", fixed = TRUE)
  expect_error(cr_spectrum(dat, area = 999999), "No ranked cancer spectrum rows found", fixed = TRUE)
  expect_error(cr_spectrum(dat, area = 410000, n = 0), "`n` must be a positive integer", fixed = TRUE)
})
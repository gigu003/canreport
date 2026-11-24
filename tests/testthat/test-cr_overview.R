make_overview_data <- function(sex_specific = FALSE, missing_rank = FALSE) {
  rows <- expand.grid(
    areacode = c(410000, 910000, 920000),
    sex = 0:2,
    KEEP.OUT.ATTRS = FALSE
  )
  rows$areacode_cn <- c("河南省", "城市地区", "农村地区")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$areacode_en <- c("Henan Province", "Urban Area", "Rural Area")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$year <- 2022
  rows$cancer <- "115"
  rows$cancer_cn <- "乳腺癌"
  rows$cancer_en <- "Breast cancer"
  rows$no_cases <- NA_real_
  rows$no_cases[rows$areacode == 410000 & rows$sex == 0] <- 1000
  rows$no_cases[rows$areacode == 410000 & rows$sex == 1] <- if (sex_specific) 0 else 560
  rows$no_cases[rows$areacode == 410000 & rows$sex == 2] <- if (sex_specific) 1000 else 440
  rows$no_cases[rows$areacode == 910000 & rows$sex == 0] <- 550
  rows$no_cases[rows$areacode == 910000 & rows$sex == 1] <- if (sex_specific) 0 else 300
  rows$no_cases[rows$areacode == 910000 & rows$sex == 2] <- if (sex_specific) 550 else 250
  rows$no_cases[rows$areacode == 920000 & rows$sex == 0] <- 450
  rows$no_cases[rows$areacode == 920000 & rows$sex == 1] <- if (sex_specific) 0 else 260
  rows$no_cases[rows$areacode == 920000 & rows$sex == 2] <- if (sex_specific) 450 else 190

  rows$cr <- NA_real_
  rows$cr[rows$areacode == 410000 & rows$sex == 0] <- 50
  rows$cr[rows$areacode == 410000 & rows$sex == 1] <- 60
  rows$cr[rows$areacode == 410000 & rows$sex == 2] <- 40
  rows$cr[rows$areacode == 910000 & rows$sex == 0] <- 55
  rows$cr[rows$areacode == 910000 & rows$sex == 1] <- 65
  rows$cr[rows$areacode == 910000 & rows$sex == 2] <- 45
  rows$cr[rows$areacode == 920000 & rows$sex == 0] <- 45
  rows$cr[rows$areacode == 920000 & rows$sex == 1] <- 50
  rows$cr[rows$areacode == 920000 & rows$sex == 2] <- 35

  rows$asr_cn2000 <- NA_real_
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 0] <- 40
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 1] <- 48
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 2] <- 32
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 0] <- 44
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 1] <- 54
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 2] <- 34
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 0] <- 36
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 1] <- 42
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 2] <- 30

  rows$asr_wld85 <- rows$asr_cn2000 - 2
  rows$cumur <- 4.5
  rows$prop <- 12.34
  rows$rank <- 1
  if (missing_rank) rows$rank[rows$areacode == 410000 & rows$sex == 0] <- NA
  rows
}

test_that("cr_overview renders Chinese incidence overview", {
  out <- cr_overview(
    make_overview_data(sex_specific = FALSE),
    cancer = "乳腺癌",
    event_type = "incidence",
    area = 410000,
    lang = "zh"
  )

  expect_match(out, "2022年，乳腺癌位居河南省肿瘤登记地区癌症发病谱的第1位", fixed = TRUE)
  expect_match(out, "新发病例数为1000例，占全部癌症发病的12.34%", fixed = TRUE)
  expect_match(out, "其中，男性560例，女性440例，城市550例，农村450例", fixed = TRUE)
  expect_match(out, "中标发病率呈现男性高于女性以及城市地区高于农村地区的特点", fixed = TRUE)
})

test_that("cr_overview renders English incidence overview", {
  out <- cr_overview(
    make_overview_data(sex_specific = FALSE),
    cancer = "Breast cancer",
    event_type = "incidence",
    area = "Henan Province",
    lang = "en"
  )

  expect_match(out, "In 2022, breast cancer was the 1st most common cancer in the registration area of Henan Province, China", fixed = TRUE)
  expect_match(out, "A total of 1000 new cases of breast cancer were diagnosed", fixed = TRUE)
  expect_match(out, "The crude incidence rate was 50.00 per 100,000 population", fixed = TRUE)
  expect_match(out, "1.50 times higher in males than in females", fixed = TRUE)
})

test_that("cr_overview renders mortality terms", {
  out_zh <- cr_overview(
    make_overview_data(sex_specific = FALSE),
    cancer = "115",
    event_type = "mortality",
    area = 410000,
    lang = "zh"
  )
  out_en <- cr_overview(
    make_overview_data(sex_specific = FALSE),
    cancer = "115",
    event_type = "mortality",
    area = 410000,
    lang = "en"
  )

  expect_match(out_zh, "死亡谱的第1位", fixed = TRUE)
  expect_match(out_zh, "死亡病例数为1000例", fixed = TRUE)
  expect_match(out_en, "most common cause of cancer deaths", fixed = TRUE)
  expect_match(out_en, "A total of 1000 cases died of breast cancer", fixed = TRUE)
})

test_that("cr_overview omits sex strata for sex-specific cancer", {
  out <- cr_overview(
    make_overview_data(sex_specific = TRUE),
    cancer = "乳腺癌",
    area = 410000,
    lang = "zh"
  )

  expect_false(grepl("男性", out, fixed = TRUE))
  expect_match(out, "其中，城市550例，农村450例", fixed = TRUE)
  expect_match(out, "中标发病率呈现城市地区高于农村地区的特点", fixed = TRUE)
})

test_that("cr_overview validates inputs", {
  dat <- make_overview_data()
  expect_error(cr_overview(dat, cancer = "乳腺癌"), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_overview(dat, cancer = "不存在", area = 410000), "No ASR row found", fixed = TRUE)
  expect_error(cr_overview(make_overview_data(missing_rank = TRUE), cancer = "115", area = 410000), "`rank` is missing", fixed = TRUE)
})

test_that("cr_overview compare_threshold can classify subgroup rates as similar", {
  out <- cr_overview(
    make_overview_data(sex_specific = FALSE),
    cancer = "115",
    area = 410000,
    lang = "zh",
    compare_threshold = 1
  )

  expect_match(out, "男性和女性水平接近", fixed = TRUE)
  expect_match(out, "城市地区和农村地区水平接近", fixed = TRUE)
})

test_that("cr_overview supports structured output modes", {
  dat <- make_overview_data(sex_specific = FALSE)

  data_out <- cr_overview(dat, cancer = "115", area = 410000, output = "data")
  expect_s3_class(data_out, "data.frame")
  expect_equal(nrow(data_out), 1L)
  expect_equal(data_out$cancer_name, "乳腺癌")
  expect_equal(data_out$no_cases, "1000")

  both_out <- cr_overview(dat, cancer = "115", area = 410000, output = "both")
  expect_type(both_out, "list")
  expect_named(both_out, c("text", "data", "values"))
  expect_type(both_out$text, "character")
  expect_s3_class(both_out$data, "data.frame")
  expect_equal(both_out$values$cancer_name, "乳腺癌")
})
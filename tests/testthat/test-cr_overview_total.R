## Run these tests through the package test entry point, for example:
## devtools::test(filter = "cr_overview_total")
## If using testthat::test_file() directly, call devtools::load_all() first.

make_overview_total_data <- function() {
  rows <- expand.grid(
    areacode = c(410000, 910000, 920000),
    sex = 0:2,
    KEEP.OUT.ATTRS = FALSE
  )
  rows$areacode_cn <- c("河南省", "城市地区", "农村地区")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$areacode_en <- c("Henan Province", "Urban Area", "Rural Area")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$year <- 2022
  rows$cancer <- "100"
  rows$cancer_cn <- "全部恶性肿瘤"
  rows$cancer_en <- "All cancers"

  rows$no_cases <- NA_real_
  rows$no_cases[rows$areacode == 410000 & rows$sex == 0] <- 176844
  rows$no_cases[rows$areacode == 410000 & rows$sex == 1] <- 91311
  rows$no_cases[rows$areacode == 410000 & rows$sex == 2] <- 85533
  rows$no_cases[rows$areacode == 910000 & rows$sex == 0] <- 46724
  rows$no_cases[rows$areacode == 910000 & rows$sex == 1] <- 23000
  rows$no_cases[rows$areacode == 910000 & rows$sex == 2] <- 23724
  rows$no_cases[rows$areacode == 920000 & rows$sex == 0] <- 130120
  rows$no_cases[rows$areacode == 920000 & rows$sex == 1] <- 68311
  rows$no_cases[rows$areacode == 920000 & rows$sex == 2] <- 61809

  rows$cr <- NA_real_
  rows$cr[rows$areacode == 410000 & rows$sex == 0] <- 299.47
  rows$cr[rows$areacode == 410000 & rows$sex == 1] <- 302.50
  rows$cr[rows$areacode == 410000 & rows$sex == 2] <- 296.32
  rows$cr[rows$areacode == 910000 & rows$sex == 0] <- 333.91
  rows$cr[rows$areacode == 910000 & rows$sex == 1] <- 330.60
  rows$cr[rows$areacode == 910000 & rows$sex == 2] <- 337.17
  rows$cr[rows$areacode == 920000 & rows$sex == 0] <- 288.78
  rows$cr[rows$areacode == 920000 & rows$sex == 1] <- 294.11
  rows$cr[rows$areacode == 920000 & rows$sex == 2] <- 283.10

  rows$asr_cn2000 <- NA_real_
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 0] <- 199.51
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 1] <- 205.00
  rows$asr_cn2000[rows$areacode == 410000 & rows$sex == 2] <- 194.00
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 0] <- 223.18
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 1] <- 230.00
  rows$asr_cn2000[rows$areacode == 910000 & rows$sex == 2] <- 216.00
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 0] <- 191.85
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 1] <- 198.00
  rows$asr_cn2000[rows$areacode == 920000 & rows$sex == 2] <- 186.00

  rows$asr_wld85 <- NA_real_
  rows$asr_wld85[rows$areacode == 410000 & rows$sex == 0] <- 193.69
  rows$asr_wld85[rows$areacode == 910000 & rows$sex == 0] <- 215.00
  rows$asr_wld85[rows$areacode == 920000 & rows$sex == 0] <- 186.77
  rows$asr_wld85[rows$sex != 0] <- rows$asr_cn2000[rows$sex != 0] - 5

  rows$cumur <- NA_real_
  rows$cumur[rows$areacode == 410000 & rows$sex == 0] <- 22.25
  rows$cumur[rows$areacode == 910000 & rows$sex == 0] <- 24.37
  rows$cumur[rows$areacode == 920000 & rows$sex == 0] <- 21.56
  rows$cumur[rows$sex != 0] <- rows$cumur[match(rows$areacode[rows$sex != 0], c(410000, 910000, 920000))]
  rows
}

test_that("cr_overview_total renders Chinese incidence overview", {
  out <- cr_overview_total(make_overview_total_data(), event_type = "incidence", area = 410000, lang = "zh")

  expect_match(out, "河南省肿瘤登记地区2022年报告恶性肿瘤新发病例数合计176844例，其中男性91311例，女性85533例", fixed = TRUE)
  expect_match(out, "城市地区上报46724例，占26.42%；农村地区报告130120例，占73.58%", fixed = TRUE)
  expect_match(out, "河南省肿瘤登记地区恶性肿瘤发病率为299.47/10万（男性302.50/10万，女性296.32/10万）", fixed = TRUE)
  expect_match(out, "中标率199.51/10万，世标率193.69/10万，累积率（0-74岁）22.25%", fixed = TRUE)
  expect_match(out, "城市与农村相比，城市地区男性、女性及男女合计中标发病率均高于农村地区", fixed = TRUE)
})

test_that("cr_overview_total renders English and mortality overview", {
  out <- cr_overview_total(
    make_overview_total_data(),
    event_type = "mortality",
    area = "Henan Province",
    lang = "en"
  )

  expect_match(out, "In 2022, a total of 176844 deaths of malignant tumors were reported in Henan Province", fixed = TRUE)
  expect_match(out, "Urban areas reported 46724 cases, accounting for 26.42%", fixed = TRUE)
  expect_match(out, "The cancer mortality rate in Henan Province was 299.47 per 100,000 population", fixed = TRUE)
  expect_match(out, "the ASR China of cancer mortality was higher in urban areas than in rural areas", fixed = TRUE)
})

test_that("cr_overview_total supports all language and event type combinations", {
  dat <- make_overview_total_data()

  zh_mortality <- cr_overview_total(dat, event_type = "mortality", area = "河南省", lang = "zh")
  expect_match(zh_mortality, "恶性肿瘤死亡病例数合计176844例", fixed = TRUE)
  expect_match(zh_mortality, "恶性肿瘤死亡率为299.47/10万", fixed = TRUE)
  expect_match(zh_mortality, "中标死亡率均高于农村地区", fixed = TRUE)

  en_incidence <- cr_overview_total(dat, event_type = "incidence", area = 410000, lang = "en")
  expect_match(en_incidence, "a total of 176844 new cases of malignant tumors", fixed = TRUE)
  expect_match(en_incidence, "The cancer incidence rate in Henan Province was 299.47 per 100,000 population", fixed = TRUE)
  expect_match(en_incidence, "the ASR China of cancer incidence was higher in urban areas than in rural areas", fixed = TRUE)
})

test_that("cr_overview_total supports explicit total cancer label and validates inputs", {
  dat <- make_overview_total_data()

  expect_match(cr_overview_total(dat, area = 410000, cancer_total = "全部恶性肿瘤", lang = "zh"), "恶性肿瘤发病率", fixed = TRUE)
  expect_match(cr_overview_total(dat, area = 410000, cancer_total = "All cancers", lang = "en"), "malignant tumors were reported", fixed = TRUE)
  expect_match(cr_overview_total(dat, area = 410000, cancer_total = "100", lang = "zh"), "恶性肿瘤发病率", fixed = TRUE)
  expect_error(cr_overview_total(dat), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_overview_total(dat, area = 999999), "No total cancer ASR row found", fixed = TRUE)
})

test_that("cr_overview_total renders urban-rural comparison branches", {
  dat <- make_overview_total_data()

  rural_higher <- dat
  rural_higher$asr_cn2000[rural_higher$areacode == 910000] <- 100
  rural_higher$asr_cn2000[rural_higher$areacode == 920000] <- 200
  expect_match(
    cr_overview_total(rural_higher, area = 410000, lang = "zh"),
    "农村地区男性、女性及男女合计中标发病率均高于城市地区",
    fixed = TRUE
  )

  similar <- dat
  expect_match(
    cr_overview_total(similar, area = 410000, lang = "en", compare_threshold = 1),
    "the ASR China of cancer incidence was similar between urban and rural areas among males, females, and both sexes combined",
    fixed = TRUE
  )

  mixed <- dat
  mixed$asr_cn2000[mixed$areacode == 910000 & mixed$sex == 0] <- 250
  mixed$asr_cn2000[mixed$areacode == 920000 & mixed$sex == 0] <- 200
  mixed$asr_cn2000[mixed$areacode == 910000 & mixed$sex == 1] <- 100
  mixed$asr_cn2000[mixed$areacode == 920000 & mixed$sex == 1] <- 200
  mixed$asr_cn2000[mixed$areacode == 910000 & mixed$sex == 2] <- 250
  mixed$asr_cn2000[mixed$areacode == 920000 & mixed$sex == 2] <- 200
  expect_match(
    cr_overview_total(mixed, area = 410000, lang = "zh"),
    "城市地区和农村地区不同性别中标发病率高低不一",
    fixed = TRUE
  )
})

test_that("cr_overview_total supports structured output modes", {
  dat <- make_overview_total_data()

  data_out <- cr_overview_total(dat, area = 410000, output = "data")
  expect_s3_class(data_out, "data.frame")
  expect_equal(nrow(data_out), 1L)
  expect_equal(data_out$total_cases, "176844")
  expect_equal(data_out$area_name, "河南省")

  both_out <- cr_overview_total(dat, area = 410000, output = "both")
  expect_type(both_out, "list")
  expect_named(both_out, c("text", "data", "values"))
  expect_match(both_out$text, "恶性肿瘤新发病例数合计176844例", fixed = TRUE)
  expect_equal(both_out$values$total_cr, "299.47")
})
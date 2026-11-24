make_age_pattern_data <- function(sex_specific = FALSE, area_trend = "similar") {
  agegrp <- factor(
    c("0 岁", "1-4 岁", "5-9 岁", "10-14 岁", "15-19 岁", "20-24 岁", "25-29 岁", "30-34 岁", "35-39 岁", "40-44 岁", "45-49 岁", "50-54 岁", "55-59 岁", "60-64 岁", "65-69 岁", "70-74 岁", "75-79 岁", "80-84 岁", "85+ 岁"),
    levels = c("0 岁", "1-4 岁", "5-9 岁", "10-14 岁", "15-19 岁", "20-24 岁", "25-29 岁", "30-34 岁", "35-39 岁", "40-44 岁", "45-49 岁", "50-54 岁", "55-59 岁", "60-64 岁", "65-69 岁", "70-74 岁", "75-79 岁", "80-84 岁", "85+ 岁")
  )
  total_rate <- c(0, 0.5, 0.8, 1, 2, 4, 8, 15, 20, 40, 80, 150, 260, 380, 480, 560, 580, 588.61, 570)
  rows <- list()
  add_rows <- function(areacode, areacode_cn, areacode_en, sex, rate_multiplier = 1, rate = NULL) {
    if (is.null(rate)) rate <- total_rate * rate_multiplier
    data.frame(
      areacode = areacode,
      areacode_cn = areacode_cn,
      areacode_en = areacode_en,
      year = 2022,
      sex = sex,
      sex_en = c("Total", "Male", "Female")[sex + 1L],
      sex_cn = c("合计", "男性", "女性")[sex + 1L],
      cancer = "110",
      cancer_en = "Trachea, Bronchus & Lung cancer",
      cancer_cn = "气管、支气管、肺癌",
      agegrp = agegrp,
      death = 1000,
      cases = if (sex_specific && sex == 1L) 0 else 10,
      rate = if (sex_specific && sex == 1L) 0 else rate,
      stringsAsFactors = FALSE
    )
  }

  rows[[1L]] <- add_rows(410000, "河南省", "Henan Province", 0)
  rows[[2L]] <- add_rows(410000, "河南省", "Henan Province", 1, 1.4)
  rows[[3L]] <- add_rows(410000, "河南省", "Henan Province", 2, 0.7)
  rows[[4L]] <- add_rows(910000, "城市地区", "Urban Area", 0, 1.2)
  rural_rate <- if (identical(area_trend, "different")) rev(total_rate) else total_rate * 0.8
  rows[[5L]] <- add_rows(920000, "农村地区", "Rural Area", 0, rate = rural_rate)
  do.call(rbind, rows)
}

test_that("cr_age_pattern renders Chinese age-specific pattern", {
  out <- cr_age_pattern(
    make_age_pattern_data(),
    cancer = "气管、支气管、肺癌",
    area = 410000,
    lang = "zh"
  )

  expect_match(out, "气管、支气管、肺癌年龄别发病率在0-39岁年龄段处于较低水平", fixed = TRUE)
  expect_match(out, "在80-84岁年龄组达到峰值，发病率为588.61/10万", fixed = TRUE)
  expect_match(out, "年龄别发病率呈现男性高于女性的特点", fixed = TRUE)
  expect_match(out, "男性在80-84岁年龄组达到峰值，发病率为824.05/10万", fixed = TRUE)
  expect_match(out, "女性在80-84岁年龄组达到峰值，发病率为412.03/10万", fixed = TRUE)
  expect_match(out, "城市地区和农村地区年龄别率虽然有一定差异，但是总体趋势类同", fixed = TRUE)
  expect_match(out, "总体来看，多数年龄组城市地区高于农村地区", fixed = TRUE)
  expect_match(out, "其中城市地区在1-4岁及以上年龄段高于农村地区", fixed = TRUE)
})

test_that("cr_age_pattern renders English and mortality terms", {
  out <- cr_age_pattern(
    make_age_pattern_data(),
    cancer = "Trachea, Bronchus & Lung cancer",
    event_type = "mortality",
    area = "Henan Province",
    lang = "en"
  )

  expect_match(out, "The age-specific mortality rate of trachea, bronchus & lung cancer remained low among those aged 0-39 years", fixed = TRUE)
  expect_match(out, "peaked in the 80-84 years age group at 588.61 per 100,000 population", fixed = TRUE)
  expect_match(out, "the age-specific mortality rate was generally higher in males than in females", fixed = TRUE)
  expect_match(out, "peaking in males in the 80-84 years age group at 824.05 per 100,000 population and in females in the 80-84 years age group at 412.03 per 100,000 population", fixed = TRUE)
  expect_match(out, "overall, rates were higher in urban areas than in rural areas in most age groups", fixed = TRUE)
  expect_match(out, "rates were higher in urban areas than in rural areas among those aged 1-4+ years", fixed = TRUE)
})

test_that("cr_age_pattern omits sex comparison for sex-specific cancer", {
  out <- cr_age_pattern(
    make_age_pattern_data(sex_specific = TRUE),
    cancer = "110",
    area = 410000,
    lang = "zh"
  )

  expect_false(grepl("男性高于女性", out, fixed = TRUE))
  expect_false(grepl("男性在", out, fixed = TRUE))
  expect_match(out, "城市地区和农村地区年龄别率", fixed = TRUE)
})

test_that("cr_age_pattern detects different urban-rural trends", {
  out <- cr_age_pattern(
    make_age_pattern_data(area_trend = "different"),
    cancer = "110",
    area = 410000,
    lang = "zh"
  )

  expect_match(out, "城市地区和农村地区年龄别率变化趋势存在差异", fixed = TRUE)
  expect_match(out, "总体来看，城市地区和农村地区在不同年龄组间互有高低", fixed = TRUE)
  expect_match(out, "城市地区在40-44岁及以上年龄段高于农村地区，农村地区在0-39岁年龄段高于城市地区", fixed = TRUE)
})

test_that("cr_age_pattern area comparison uses total sex only", {
  dat <- make_age_pattern_data()
  dat$rate[dat$areacode == 910000 & dat$sex %in% c(1, 2)] <- 0
  dat$rate[dat$areacode == 920000 & dat$sex %in% c(1, 2)] <- 9999

  out <- cr_age_pattern(
    dat,
    cancer = "110",
    area = 410000,
    lang = "zh"
  )

  expect_match(out, "城市地区在1-4岁及以上年龄段高于农村地区", fixed = TRUE)
})

test_that("cr_age_pattern validates inputs", {
  dat <- make_age_pattern_data()
  expect_error(cr_age_pattern(dat, cancer = "110"), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_age_pattern(dat, cancer = "不存在", area = 410000), "No age-specific rate rows found", fixed = TRUE)
  expect_error(cr_age_pattern(dat, cancer = "110", area = 410000, low_rate_threshold = 2), "`low_rate_threshold` must be a number between 0 and 1", fixed = TRUE)
})
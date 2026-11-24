make_age_pattern_total_data <- function() {
  agegrp <- factor(
    c("0 岁", "1-4 岁", "5-9 岁", "10-14 岁", "15-19 岁", "20-24 岁", "25-29 岁", "30-34 岁", "35-39 岁", "40-44 岁", "45-49 岁", "50-54 岁", "55-59 岁", "60-64 岁", "65-69 岁", "70-74 岁", "75-79 岁", "80-84 岁", "85+ 岁"),
    levels = c("0 岁", "1-4 岁", "5-9 岁", "10-14 岁", "15-19 岁", "20-24 岁", "25-29 岁", "30-34 岁", "35-39 岁", "40-44 岁", "45-49 岁", "50-54 岁", "55-59 岁", "60-64 岁", "65-69 岁", "70-74 岁", "75-79 岁", "80-84 岁", "85+ 岁")
  )
  total_rate <- c(5, 6, 8, 10, 15, 25, 45, 80, 120, 180, 260, 360, 480, 620, 760, 880, 950, 990, 1000)
  male_rate <- total_rate
  female_rate <- total_rate
  female_rate[5:13] <- male_rate[5:13] * 1.2
  male_rate[14:19] <- female_rate[14:19] * 1.3

  rows <- list()
  add_rows <- function(areacode, areacode_cn, areacode_en, sex, rate) {
    data.frame(
      areacode = areacode,
      areacode_cn = areacode_cn,
      areacode_en = areacode_en,
      year = 2022,
      sex = sex,
      sex_en = c("Total", "Male", "Female")[sex + 1L],
      sex_cn = c("合计", "男性", "女性")[sex + 1L],
      cancer = "60",
      cancer_en = "All malignant tumors",
      cancer_cn = "恶性肿瘤",
      agegrp = agegrp,
      death = 1000,
      cases = 10,
      rate = rate,
      stringsAsFactors = FALSE
    )
  }

  urban_multiplier <- c(rep(1.1, 15), rep(1.35, 4))
  rural_multiplier <- rep(1, length(total_rate))

  rows[[1L]] <- add_rows(410000, "河南省", "Henan Province", 0, total_rate)
  rows[[2L]] <- add_rows(410000, "河南省", "Henan Province", 1, male_rate)
  rows[[3L]] <- add_rows(410000, "河南省", "Henan Province", 2, female_rate)
  rows[[4L]] <- add_rows(910000, "城市地区", "Urban Area", 0, total_rate * urban_multiplier)
  rows[[5L]] <- add_rows(910000, "城市地区", "Urban Area", 1, male_rate * urban_multiplier)
  rows[[6L]] <- add_rows(910000, "城市地区", "Urban Area", 2, female_rate * urban_multiplier)
  rows[[7L]] <- add_rows(920000, "农村地区", "Rural Area", 0, total_rate * rural_multiplier)
  rows[[8L]] <- add_rows(920000, "农村地区", "Rural Area", 1, male_rate * rural_multiplier)
  rows[[9L]] <- add_rows(920000, "农村地区", "Rural Area", 2, female_rate * rural_multiplier)
  do.call(rbind, rows)
}

test_that("cr_age_pattern_total renders Chinese total cancer age-specific pattern", {
  out <- cr_age_pattern_total(
    make_age_pattern_total_data(),
    area = 410000,
    cancer = 60,
    event_type = "incidence",
    lang = "zh"
  )

  expect_match(out, "河南省肿瘤登记地区2022年恶性肿瘤年龄别发病率在0-29岁年龄组处于较低水平", fixed = TRUE)
  expect_match(out, "30岁及以上年龄组发病率明显升高", fixed = TRUE)
  expect_match(out, "男性和女性年龄别发病率随年龄变化趋势基本一致", fixed = TRUE)
  expect_match(out, "15-59岁年龄组女性发病率高于男性", fixed = TRUE)
  expect_match(out, "60岁及以上年龄组则男性发病率高于女性", fixed = TRUE)
  expect_match(out, "0-69岁多数年龄组城市地区年龄别率略高于农村地区", fixed = TRUE)
  expect_match(out, "70岁及以上各年龄组城市地区发病率明显高于农村地区", fixed = TRUE)
  expect_match(out, "男性和女性年龄别发病率的城乡分布特征基本一致", fixed = TRUE)
})

test_that("cr_age_pattern_total renders English and mortality terms", {
  out <- cr_age_pattern_total(
    make_age_pattern_total_data(),
    area = "Henan Province",
    cancer = "All malignant tumors",
    event_type = "mortality",
    lang = "en"
  )

  expect_match(out, "In 2022, the age-specific mortality rate of all malignant tumors in Henan Province remained low among those aged 0-29 years", fixed = TRUE)
  expect_match(out, "increased markedly from age 30 years onward", fixed = TRUE)
  expect_match(out, "rates were higher in females than in males among those aged 15-59 years", fixed = TRUE)
  expect_match(out, "rates were higher in males than in females among those aged 60+ years", fixed = TRUE)
  expect_match(out, "urban rates were markedly higher than rural rates among those aged 70+ years", fixed = TRUE)
})

test_that("cr_age_pattern_total validates required arguments", {
  dat <- make_age_pattern_total_data()
  expect_error(cr_age_pattern_total(dat, cancer = 60), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_age_pattern_total(dat, area = 410000), "`cancer` must be supplied", fixed = TRUE)
  expect_error(cr_age_pattern_total(dat, area = 410000, cancer = 999), "No age-specific rate rows found", fixed = TRUE)
})
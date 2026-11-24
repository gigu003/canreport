make_coverage_data <- function() {
  rows <- expand.grid(
    areacode = c(410000, 910000, 920000),
    sex = 1:2,
    agegrp = c("0 岁", "1-4 岁"),
    KEEP.OUT.ATTRS = FALSE
  )
  rows$areacode_cn <- c("河南省", "城市地区", "农村地区")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$areacode_en <- c("Henan Province", "Urban Area", "Rural Area")[match(rows$areacode, c(410000, 910000, 920000))]
  rows$year <- 2022
  rows$sex_cn <- c("男性", "女性")[match(rows$sex, 1:2)]
  rows$sex_en <- c("Male", "Female")[match(rows$sex, 1:2)]
  rows$death <- 0
  rows$rks <- NA_real_

  rows$rks[rows$areacode == 410000 & rows$sex == 1] <- c(10000000, 20185949)
  rows$rks[rows$areacode == 410000 & rows$sex == 2] <- c(10000000, 18865447)
  rows$rks[rows$areacode == 910000 & rows$sex == 1] <- c(3000000, 3936864)
  rows$rks[rows$areacode == 910000 & rows$sex == 2] <- c(3000000, 4056023)
  rows$rks[rows$areacode == 920000 & rows$sex == 1] <- c(10000000, 13249085)
  rows$rks[rows$areacode == 920000 & rows$sex == 2] <- c(10000000, 11809424)
  rows
}

test_that("cr_coverage renders Chinese coverage paragraph", {
  out <- cr_coverage(
    make_coverage_data(),
    area = 410000,
    pop_total = 97994351,
    report_year = 2022,
    pop_source = "2023 年河南省统计年鉴：9 872万",
    lang = "zh"
  )

  expect_match(out, "2022年，纳入年报分析的登记地区人口为59 051 396人", fixed = TRUE)
  expect_match(out, "其中男性30 185 949人，女性28 865 447人", fixed = TRUE)
  expect_match(out, "约占2022年河南省总人口的60.26%", fixed = TRUE)
  expect_match(out, "按地区划分，城市地区人口为13 992 887人，占登记地区总人口的23.70%；农村地区人口为45 058 509人，占76.30%", fixed = TRUE)
})

test_that("cr_coverage renders English coverage paragraph", {
  out <- cr_coverage(
    make_coverage_data(),
    area = "Henan Province",
    pop_total = 98720000,
    report_year = 2025,
    pop_source = "2023 Henan Statistical Yearbook: 98.72 million",
    lang = "en"
  )

  expect_match(out, "In 2025, the population covered by the registration areas", fixed = TRUE)
  expect_match(out, "was 59 051 396, including 30 185 949 males and 28 865 447 females", fixed = TRUE)
  expect_match(out, "accounting for approximately 59.82% of the total population of Henan Province", fixed = TRUE)
  expect_match(out, "The urban areas covered 13 992 887 people", fixed = TRUE)
})

test_that("cr_coverage supports defaults and validates inputs", {
  dat <- make_coverage_data()

  expect_match(
    cr_coverage(dat, area = "河南省", pop_total = 98720000, lang = "zh"),
    "2022年，纳入年报分析的登记地区人口为59 051 396人",
    fixed = TRUE
  )
  expect_error(cr_coverage(dat, pop_total = 98720000), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_coverage(dat, area = 410000), "`pop_total` must be supplied", fixed = TRUE)
  expect_error(cr_coverage(dat, area = 410000, pop_total = -1), "`pop_total` must be a positive numeric scalar", fixed = TRUE)
  expect_error(cr_coverage(dat, area = 999999, pop_total = 98720000), "No population rows found", fixed = TRUE)
  expect_error(cr_coverage(dat[, setdiff(names(dat), "rks")], area = 410000, pop_total = 98720000), "missing required columns", fixed = TRUE)
})
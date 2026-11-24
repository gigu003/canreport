make_qua_summary_data <- function() {
  add_row <- function(areacode, areacode_cn, areacode_en, sex, sex_cn, sex_en,
                      mv, mi, dco, ub = 2.38, sub = 35.14, mvs = 13.34) {
    data.frame(
      areacode = areacode,
      areacode_cn = areacode_cn,
      areacode_en = areacode_en,
      year = 2022,
      sex = sex,
      sex_cn = sex_cn,
      sex_en = sex_en,
      cancer = "60",
      cancer_cn = "恶性肿瘤",
      cancer_en = "All Cancers",
      rks = 1000000,
      fbs = 1000,
      inci = 100,
      sws = 500,
      mort = 50,
      mi = mi,
      mv = mv,
      dco = dco,
      ub = ub,
      sub = sub,
      mvs = mvs,
      ids = 0,
      mult = 0,
      stringsAsFactors = FALSE
    )
  }

  rows <- list(
    add_row(410000, "河南省", "Henan Province", 0, "合计", "Total", 74.83, 0.58, 1.64),
    add_row(410000, "河南省", "Henan Province", 1, "男性", "Male", 72.76, 0.70, 2.08),
    add_row(410000, "河南省", "Henan Province", 2, "女性", "Female", 77.28, 0.43, 1.13),
    add_row(910000, "城市地区", "Urban Area", 0, "合计", "Total", 78.81, 0.51, 1.31),
    add_row(920000, "农村地区", "Rural Area", 0, "合计", "Total", 73.40, 0.60, 1.76)
  )
  do.call(rbind, rows)
}

test_that("cr_qua_summary renders Chinese quality summary", {
  out <- cr_qua_summary(make_qua_summary_data(), area = 410000, cancer = 60, lang = "zh")

  expect_match(out, "2022年河南省肿瘤登记地区恶性肿瘤病理诊断比例（MV%）为74.83%", fixed = TRUE)
  expect_match(out, "死亡发病比（M:I）为0.58", fixed = TRUE)
  expect_match(out, "仅有死亡医学证明书比例（DCO%）为1.64%", fixed = TRUE)
  expect_match(out, "部位不明比例为2.38%", fixed = TRUE)
  expect_match(out, "亚部位不明比例为35.14%", fixed = TRUE)
  expect_match(out, "男性MV%为72.76%，M:I为0.70，DCO%为2.08%；女性MV%为77.28%，M:I为0.43，DCO%为1.13%", fixed = TRUE)
  expect_match(out, "城市地区MV%为78.81%，M:I为0.51，DCO%为1.31%；农村地区MV%为73.40%，M:I为0.60，DCO%为1.76%", fixed = TRUE)
  expect_match(out, "城市地区MV%高于农村地区，M:I低于农村地区", fixed = TRUE)
  expect_false(grepl("形态学类型不明确", out, fixed = TRUE))
  expect_false(grepl("身份证", out, fixed = TRUE))
  expect_false(grepl("多原发", out, fixed = TRUE))
})

test_that("cr_qua_summary renders English quality summary", {
  out <- cr_qua_summary(make_qua_summary_data(), area = "Henan Province", cancer = "All Cancers", lang = "en")

  expect_match(out, "In 2022, the proportion of morphologically verified cases (MV%) for all cancers", fixed = TRUE)
  expect_match(out, "the mortality-to-incidence ratio (M:I) was 0.58", fixed = TRUE)
  expect_match(out, "Among males, MV% was 72.76%, M:I was 0.70, and DCO% was 2.08%", fixed = TRUE)
  expect_match(out, "In urban areas, MV% was 78.81%, M:I was 0.51, and DCO% was 1.31%", fixed = TRUE)
  expect_match(out, "MV% was higher in urban areas than in rural areas, and M:I was lower in urban areas than in rural areas", fixed = TRUE)
  expect_false(grepl("unspecified morphology", out, fixed = TRUE))
})

test_that("cr_qua_summary validates inputs", {
  dat <- make_qua_summary_data()
  expect_error(cr_qua_summary(dat), "`area` must be supplied", fixed = TRUE)
  expect_error(cr_qua_summary(dat, area = 999999), "No quality indicator row found", fixed = TRUE)
  expect_error(cr_qua_summary(dat[, setdiff(names(dat), "mv")], area = 410000), "missing required columns", fixed = TRUE)
  expect_error(cr_qua_summary(dat, area = 410000, compare_threshold = 2), "`compare_threshold` must be a number between 0 and 1", fixed = TRUE)
})

test_that("cr_qua_summary supports structured output modes", {
  dat <- make_qua_summary_data()

  data_out <- cr_qua_summary(dat, area = 410000, output = "data")
  expect_s3_class(data_out, "data.frame")
  expect_equal(nrow(data_out), 1L)
  expect_equal(data_out$total_mv, "74.83")
  expect_equal(data_out$total_mi, "0.58")

  both_out <- cr_qua_summary(dat, area = 410000, output = "both")
  expect_type(both_out, "list")
  expect_named(both_out, c("text", "data", "values"))
  expect_match(both_out$text, "病理诊断比例", fixed = TRUE)
  expect_equal(both_out$values$total_dco, "1.64")
})
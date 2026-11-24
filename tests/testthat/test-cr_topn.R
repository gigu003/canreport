make_topn_data <- function() {
  areas <- data.frame(
    areacode = 1:5,
    areacode_cn = c("卧龙区", "宛城区", "鹤壁市城区", "济源市", "安阳市城区"),
    areacode_en = c("Wolong District", "Wancheng District", "Hebi Urban", "Jiyuan City", "Anyang Urban")
  )

  rates <- list(
    total = c(31.36, 25.92, 19.51, 19.38, 15.63),
    male = c(42.98, 36.74, 25.18, 25.03, 20.88),
    female = c(19.69, 15.03, 14.32, 14.35, 10.46)
  )

  rows <- lapply(seq_along(rates), function(i) {
    x <- areas
    x$sex <- i - 1L
    x$cancer <- "103"
    x$cancer_cn <- "食管癌"
    x$cancer_en <- "Esophagus cancer"
    x$no_cases <- 100
    x$asr_cn2000 <- rates[[i]]
    x
  })

  do.call(rbind, rows)
}

test_that("cr_topn renders Chinese topn paragraph from templates", {
  out <- cr_topn(
    make_topn_data(),
    cancer = "食管癌",
    area_scope = "城市肿瘤登记地区",
    lang = "zh"
  )

  expect_match(out, "在5个城市肿瘤登记地区中，食管癌标化发病率最高的登记处是卧龙区，其次是宛城区，鹤壁市城区，济源市和安阳市城区", fixed = TRUE)
  expect_match(out, "其值分别为31.36/10万，25.92/10万，19.51/10万，19.38/10万和15.63/10万", fixed = TRUE)
  expect_match(out, "女性标化发病率最高的登记处是卧龙区，其次是宛城区，济源市，鹤壁市城区和安阳市城区", fixed = TRUE)
})

test_that("cr_topn renders English topn paragraph from templates", {
  out <- cr_topn(
    make_topn_data(),
    cancer = "Esophagus cancer",
    area_scope = "urban cancer registration areas",
    lang = "en"
  )

  expect_match(out, "Among the 5 urban cancer registration areas, the esophagus cancer incidence of ASR China was the highest in Wolong District, followed by Wancheng District, Hebi Urban, Jiyuan City and Anyang Urban", fixed = TRUE)
  expect_match(out, "31.36 per 100,000 population, 25.92 per 100,000 population, 19.51 per 100,000 population, 19.38 per 100,000 population and 15.63 per 100,000 population", fixed = TRUE)
  expect_match(out, "For females, the ASR China was highest in Wolong District, followed by Wancheng District, Jiyuan City, Hebi Urban and Anyang Urban", fixed = TRUE)
})

test_that("cr_topn supports mortality terms and area filtering", {
  dat <- make_topn_data()
  out <- cr_topn(
    dat,
    cancer = "103",
    event_type = "mortality",
    areas = c("宛城区", "济源市", "安阳市城区"),
    n = 2,
    lang = "zh"
  )

  expect_match(out, "在3个肿瘤登记地区中", fixed = TRUE)
  expect_match(out, "食管癌标化死亡率最高", fixed = TRUE)
  expect_match(out, "宛城区，其次是济源市", fixed = TRUE)
})

test_that("cr_topn counts actual matched areas for area_scope", {
  dat <- make_topn_data()
  out <- cr_topn(
    dat,
    cancer = "食管癌",
    areas = c("卧龙区", "卧龙区", "宛城区", "不存在地区"),
    area_scope = "城市肿瘤登记地区",
    n = 2,
    lang = "zh"
  )

  expect_match(out, "在2个城市肿瘤登记地区中", fixed = TRUE)
})

test_that("cr_topn can omit the area scope phrase", {
  out_zh <- cr_topn(
    make_topn_data(),
    cancer = "食管癌",
    area_scope = "城市肿瘤登记地区",
    include_area_scope = FALSE,
    lang = "zh"
  )
  out_en <- cr_topn(
    make_topn_data(),
    cancer = "Esophagus cancer",
    area_scope = "urban cancer registration areas",
    include_area_scope = FALSE,
    lang = "en"
  )

  expect_false(grepl("在5个城市肿瘤登记地区中", out_zh, fixed = TRUE))
  expect_match(out_zh, "食管癌标化发病率最高的登记处是卧龙区", fixed = TRUE)
  expect_false(grepl("Among the 5 urban cancer registration areas", out_en, fixed = TRUE))
  expect_match(out_en, "The esophagus cancer incidence of ASR China was the highest in Wolong District", fixed = TRUE)
})

test_that("cr_topn omits sex strata for sex-specific cancer", {
  dat <- make_topn_data()
  dat$cancer <- "115"
  dat$cancer_cn <- "乳腺癌"
  dat$cancer_en <- "Breast cancer"
  dat$no_cases[dat$sex == 1L] <- 0
  dat$asr_cn2000[dat$sex == 1L] <- 0

  out_zh <- cr_topn(
    dat,
    cancer = "乳腺癌",
    area_scope = "城市肿瘤登记地区",
    lang = "zh"
  )
  out_en <- cr_topn(
    dat,
    cancer = "Breast cancer",
    area_scope = "urban cancer registration areas",
    lang = "en"
  )

  expect_match(out_zh, "在5个城市肿瘤登记地区中", fixed = TRUE)
  expect_false(grepl("从不同性别来看", out_zh, fixed = TRUE))
  expect_false(grepl("男性", out_zh, fixed = TRUE))
  expect_match(out_en, "Among the 5 urban cancer registration areas", fixed = TRUE)
  expect_false(grepl("For males", out_en, fixed = TRUE))
  expect_false(grepl("For females", out_en, fixed = TRUE))
})

test_that("cr_topn can force sex strata for sex-specific cancer", {
  dat <- make_topn_data()
  dat$no_cases[dat$sex == 1L] <- 0

  out <- cr_topn(
    dat,
    cancer = "食管癌",
    area_scope = "城市肿瘤登记地区",
    sex_specific = FALSE,
    lang = "zh"
  )

  expect_match(out, "从不同性别来看", fixed = TRUE)
  expect_match(out, "男性食管癌标化发病率最高", fixed = TRUE)
})

test_that("cr_topn omits area scope phrase for sex-specific cancer", {
  dat <- make_topn_data()
  dat$cancer <- "115"
  dat$cancer_cn <- "乳腺癌"
  dat$cancer_en <- "Breast cancer"
  dat$no_cases[dat$sex == 1L] <- 0

  out <- cr_topn(
    dat,
    cancer = "乳腺癌",
    area_scope = "城市肿瘤登记地区",
    include_area_scope = FALSE,
    lang = "zh"
  )

  expect_false(grepl("在5个城市肿瘤登记地区中", out, fixed = TRUE))
  expect_false(grepl("从不同性别来看", out, fixed = TRUE))
  expect_match(out, "乳腺癌标化发病率最高的登记处是卧龙区", fixed = TRUE)
})

test_that("cr_topn validates inputs", {
  dat <- make_topn_data()

  expect_error(cr_topn(dat, cancer = "食管癌", n = 0), "`n` must be a positive integer", fixed = TRUE)
  expect_error(cr_topn(dat, cancer = "食管癌", include_area_scope = NA), "`include_area_scope` must be TRUE or FALSE", fixed = TRUE)
  expect_error(cr_topn(dat, cancer = "食管癌", rate_col = "missing"), "`rate_col` is not found", fixed = TRUE)
  expect_error(cr_topn(dat, cancer = "肺癌"), "No ASR rows found", fixed = TRUE)
})

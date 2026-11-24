## code to prepare `asr` dataset goes here
library(canregtools)
library(hnreport)
fbswicds <- cr_filter(hnreport::hnfbswicd, year == 2022)
fbsw_2022 <- cr_reframe(fbswicds, strat = c("area_type", "province", "registry"))

asr <- create_asr(fbsw_2022, year, sex, cancer) |> 
  add_labels(label_type = c("full", "abbr", "full"), suffix = T, lang = "zh") |> 
  add_labels(label_type = c("full", "abbr", "full"), suffix = T, lang = "en")

usethis::use_data(asr, overwrite = TRUE)

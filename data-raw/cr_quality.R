## code to prepare `cr_quality` dataset goes here
library(canregtools)
library(hnreport)
fbswicds <- cr_filter(hnreport::hnfbswicd, year == 2022, year >= 2018)
fbsw_2022 <- cr_reframe(fbswicds, strat = c("area_type", "province", "registry"))

cr_quality <- create_quality(fbsw_2022, year, sex, cancer) |> 
  add_labels(label_type = c("full", "abbr", "full"), suffix = T, lang = "zh") |> 
  add_labels(label_type = c("full", "abbr", "full"), suffix = T, lang = "en")
usethis::use_data(cr_quality, overwrite = TRUE)

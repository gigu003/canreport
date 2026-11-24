## code to prepare `pop` dataset goes here
library(canregtools)
library(hnreport)
fbswicds <- cr_filter(hnreport::hnfbswicd, year == 2022)
fbsw_2022 <- cr_reframe(fbswicds, strat = c("area_type", "province", "registry"))
cr_pop <- get_pop(fbsw_2022, collapse = T) |> 
  add_labels(lang = "zh") |> 
  add_labels(lang = "en")


usethis::use_data(cr_pop, overwrite = TRUE)

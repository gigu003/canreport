## code to prepare `sitemorp` dataset goes here
library(canregtools)
library(hnreport)
fbswicds <- cr_filter(hnreport::hnfbswicd, year == 2022)
fbsw_2022 <- cr_reframe(fbswicds, strat = c("area_type", "province", "registry"))

site_morp <- create_site_morp(fbsw_2022, year, sex, cancer, type = "morp") |> 
  add_labels(vars = c("areacode", "sex", "cancer", "morp"), label_type = "full", lang = "zh", suffix = T) |> 
  add_labels(vars = c("areacode", "sex", "cancer", "morp"), label_type = "full", lang = "en", suffix = T)
site_icd10 <- create_site_morp(fbsw_2022, year, sex, cancer, type = "icd10") |> 
  add_labels(vars = c("areacode", "sex", "cancer", "icd10"), label_type = "full", lang = "zh", suffix = T) |> 
  add_labels(vars = c("areacode", "sex", "cancer", "icd10"), label_type = "full", lang = "en", suffix = T)
  
usethis::use_data(site_morp, site_icd10, overwrite = TRUE)

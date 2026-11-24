test_that("all exported text generators have Chinese and English templates", {
  template_names <- c("overview", "overview_total", "topn", "age_pattern", "age_pattern_total", "subsite", "spectrum", "qua_summary")

  for (name in template_names) {
    expect_true(file.exists(.cr_resource_path("inst", "templates", "zh", paste0(name, ".yml"))))
    expect_true(file.exists(.cr_resource_path("inst", "templates", "en", paste0(name, ".yml"))))
  }
})

test_that("template structures contain required keys", {
  required <- list(
    overview = list(overall = c("default", "sex_specific"), ratio = c("sex", "area")),
    overview_total = list(overall = c("default"), comparison = c("all_urban_higher", "all_rural_higher", "all_similar", "mixed")),
    topn = list(overall = c("default", "default_no_scope", "sex_specific", "sex_specific_no_scope")),
    age_pattern = list(overall = c("default", "sex_specific"), sex = c("male_higher", "female_higher", "similar")),
    age_pattern_total = list(overall = c("default"), sex_trend = c("similar", "different"), area_sex_trend = c("similar", "different"), area_sex_distribution = c("similar", "different"), area_comparison = c("urban_slight", "urban_obvious", "rural_higher", "similar")),
    subsite = list(overall = c("default", "single")),
    spectrum = list(overall = c("total", "male", "female")),
    qua_summary = list(overall = c("default"), comparison = c("mv", "mi"))
  )

  for (lang in c("zh", "en")) {
    for (name in names(required)) {
      template <- .cr_load_template(name, lang = lang)
      for (section in names(required[[name]])) {
        expect_true(section %in% names(template), info = paste(lang, name, section))
        expect_true(
          all(required[[name]][[section]] %in% names(template[[section]])),
          info = paste(lang, name, section)
        )
      }
    }
  }
})

test_that("language dictionaries contain required shared keys", {
  for (lang in c("zh", "en")) {
    lang_data <- .cr_load_lang(lang)
    expect_true(all(c("incidence", "mortality") %in% names(lang_data$event_type)))
    expect_true(all(c("male", "female", "male_higher", "female_higher", "similar") %in% names(lang_data$sex)))
    expect_true(all(c("urban", "rural", "urban_full", "rural_full", "urban_higher", "rural_higher", "similar") %in% names(lang_data$area)))
    expect_true(all(c("per_100000", "asr_china", "asr_world", "cumulative_0_74") %in% names(lang_data$phrase)))
    expect_true(all(c("comma", "and", "followed_by") %in% names(lang_data$list)))
    expect_true(all(c("full_stop", "semicolon") %in% names(lang_data$punctuation)))
    expect_true("exclude_cancer_pattern" %in% names(lang_data$spectrum))
  }
})

test_that("renderer fails fast on missing template values", {
  expect_error(
    .cr_render_text("Hello {name}, {missing}.", list(name = "world")),
    "missing",
    fixed = TRUE
  )
})

test_that("template strings are non-empty and placeholders use snake case", {
  template_names <- c("overview", "overview_total", "topn", "age_pattern", "age_pattern_total", "subsite", "spectrum", "qua_summary")
  placeholder_pattern <- "\\{([^}]+)\\}"
  snake_case_pattern <- "^[a-z][a-z0-9_]*$"

  walk_template <- function(x) {
    if (is.list(x)) {
      unlist(lapply(x, walk_template), use.names = FALSE)
    } else {
      as.character(x)
    }
  }

  for (lang in c("zh", "en")) {
    for (name in template_names) {
      template <- .cr_load_template(name, lang = lang)
      strings <- walk_template(template)

      expect_true(all(nzchar(trimws(strings))), info = paste(lang, name, "empty template string"))

      for (string in strings) {
        matches <- gregexpr(placeholder_pattern, string, perl = TRUE)
        placeholders <- regmatches(string, matches)[[1L]]
        if (length(placeholders) == 0L || identical(placeholders, character(0L))) {
          next
        }
        names <- sub("^\\{(.+)\\}$", "\\1", placeholders)
        expect_true(all(grepl(snake_case_pattern, names)), info = paste(lang, name, string))
      }
    }
  }
})

test_that("Chinese and English templates share the same top-level structure", {
  template_names <- c("overview", "overview_total", "topn", "age_pattern", "age_pattern_total", "subsite", "spectrum", "qua_summary")

  structure_paths <- function(x, prefix = character()) {
    if (!is.list(x)) {
      return(paste(prefix, collapse = "/"))
    }
    unlist(
      Map(function(value, name) structure_paths(value, c(prefix, name)), x, names(x)),
      use.names = FALSE
    )
  }

  for (name in template_names) {
    zh_paths <- sort(structure_paths(.cr_load_template(name, lang = "zh")))
    en_paths <- sort(structure_paths(.cr_load_template(name, lang = "en")))
    expect_equal(zh_paths, en_paths, info = name)
  }
})
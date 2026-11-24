
<!-- README.md is generated from README.Rmd. Please edit README.Rmd instead. -->

# canreport <img src="man/figures/logo.png" align="right" height="120" alt="canreport logo" />

`canreport` provides tools for generating narrative reports from
Population-Based Cancer Registry (PBCR) analysis outputs.

The package is designed to work with structured report data, such as
output from `canregtools`, and supports reproducible text generation
workflows for `knitr` and R Markdown reports. It helps produce
standardized, consistent, and bilingual cancer registry report text from
statistical results.

## Installation

canreport requires R 4.1 or later. Install the development version from
GitHub:

``` r
# install.packages("remotes")
remotes::install_github("gigu003/canreport")
```

From a local source checkout, run `devtools::install()` in the package
root.

## Quick start

Use the bundled example tables to generate report text and inspect the
values used in each paragraph.

### Generate an incidence overview

``` r
library(canreport)

overview <- cr_overview(
  asr,
  cancer = "Breast cancer",
  event_type = "incidence",
  area = 410000,
  lang = "en",
  output = "both"
)

cat(overview$text)
#> In 2022, breast cancer was the 2nd most common cancer in the registration area of Henan Province, China. A total of 14173 new cases of breast cancer were diagnosed, accounting for 8.01% of all new cancer cases. Among them, 4550 were in urban areas and 9623 were in rural areas. The crude incidence rate was 49.10 per 100,000 population, with an ASR China of 36.99 per 100,000 population and an ASR World of 34.32 per 100,000 population, respectively. The cumulative incidence rate among subjects aged 0 to 74 years was 3.68%. The incidence rates in urban and rural areas were 64.48 per 100,000 population and 44.12 per 100,000 population, respectively; subgroup analysis showed that the ASR China incidence rates were 1.35 times higher in urban areas than in rural areas.
```

`output = "both"` returns the paragraph in `text`, a one-row data frame
in `data`, and a named list of template values in `values`. Select the
fields you want to review alongside the narrative:

``` r
overview$data[c("year", "no_cases", "cr", "asr_cn2000", "rank")]
#>   year no_cases    cr asr_cn2000 rank
#> 1 2022    14173 49.10      36.99    2
```

For `cr_overview()`, `cr_overview_total()`, and `cr_qua_summary()`,
`output = "text"` is the default and `output = "data"` returns the
one-row table alone.

### Describe an age-specific incidence pattern

``` r
paragraph <- cr_age_pattern(
  agerate,
  cancer = "Lung cancer",
  event_type = "incidence",
  area = 410000,
  lang = "zh"
)
cat(paragraph)
#> 肺癌年龄别发病率在0-44岁年龄段处于较低水平，随后快速上升，在85岁及以上年龄组达到峰值，发病率为390.07/10万；年龄别发病率呈现男性高于女性的特点，男性在80-84岁年龄组达到峰值，发病率为588.61/10万，女性在85岁及以上年龄组达到峰值，发病率为298.82/10万；城市地区和农村地区年龄别率虽然有一定差异，但是总体趋势类同；总体来看，城市地区和农村地区在不同年龄组间互有高低。其中城市地区在10-19岁、25-69岁年龄段高于农村地区，农村地区在1-9岁、20-24岁、70-74岁及以上年龄段高于城市地区。
```

To include the paragraph in R Markdown, put `cat(paragraph)` in a chunk
with `results = "asis"`. In Quarto, use the chunk option
`#| output: asis`.

## Choose an input table and function

| Example table | What it contains | Report functions |
|----|----|----|
| `asr` | Case or death counts, crude and age-standardized rates, and ranks | `cr_overview()`, `cr_overview_total()`, `cr_topn()`, `cr_spectrum()` |
| `agerate` | Counts, denominators, and rates by age group | `cr_age_pattern()`, `cr_age_pattern_total()` |
| `cr_pop` | Population counts by area, sex, and age group | `cr_coverage()` |
| `cr_quality` | Registry quality indicators such as MV%, DCO%, and M/I | `cr_qua_summary()` |
| `site_icd10`, `site_morp` | Counts by ICD-10 subsite or morphology group | `cr_subsite()` |

For your own data, prepare one analysis year and one event type per
input table before generating a paragraph. Match the columns and strata
in the corresponding example table. For instance, prepare an ASR table
with `canregtools::create_asr()` and `canregtools::add_labels()`.
Overview paragraphs use the main area’s total, male, and female rows and
the urban and rural comparison rows. Use `area_roles` if your urban and
rural codes differ from the defaults of `910000` and `920000`.

Use `lang = "zh"` for Chinese and `lang = "en"` for English in the
narrative functions. Set `event_type` to match the input table’s
incidence or mortality statistics. See the help page for each function
for the required columns and available arguments, for example
`?canreport::cr_overview`.

## Report templates and chapters

Narrative templates live in [`inst/templates/`](inst/templates/) and
share labels and wording from [`inst/lang/`](inst/lang/). Use
`cr_chapters("methods", lang = "en")` to retrieve the built-in methods
chapter as Markdown source. It returns the chapter with its embedded
code chunks, which run when the report containing them is rendered.

## License

MIT © Qiong Chen

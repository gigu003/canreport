
<!-- README.md is generated from README.Rmd. Please edit README.Rmd instead. -->

# canreport <img src="man/figures/logo.png" align="right" height="120" alt="canreport logo" />

`canreport` provides tools for generating narrative reports from
Population-Based Cancer Registry (PBCR) analysis outputs.

The package is designed to work with structured report data, such as
output from `canregtools`, and supports reproducible text generation
workflows for `knitr` and R Markdown reports. It helps produce
standardized, consistent, and bilingual cancer registry report text from
statistical results.

## Features

- Generate overview paragraphs for cancer incidence and mortality
  indicators.
- Describe top-ranked registry areas by standardized rates.
- Describe age-specific rate patterns and subgroup comparisons.
- Summarize population coverage, cancer spectrum, subsite composition,
  and data quality indicators.
- Support Chinese and English report text through external templates.
- Keep data processing, text templates, and language resources
  separated.
- Return either rendered text or structured values for selected core
  functions.

## Installation

You can install the development version from a local checkout:

``` r
# From the package root directory
install.packages(".", repos = NULL, type = "source")
```

If you use `devtools`:

``` r
devtools::install()
```

## Example data

The package includes example datasets:

- `asr`: an age-standardized rate table generated from a
  `canregtools::create_asr()` workflow and labelled by
  `canregtools::add_labels()`.
- `agerate`: an age-specific rate table generated from a
  `canregtools::create_age_rate()` workflow and labelled by
  `canregtools::add_labels()`.
- `cr_pop`: an example population table.
- `cr_quality`: an example cancer registry quality indicator table.
- `site_icd10` and `site_morp`: example subsite and morphology count
  tables.

## Usage

``` r
library(canreport)

cr_overview(
  asr,
  cancer = "Breast cancer",
  event_type = "incidence",
  area = 410000,
  lang = "en"
)
```

``` r
cr_topn(
  asr,
  cancer = "Esophagus cancer",
  event_type = "incidence",
  area_scope = "urban cancer registration areas",
  lang = "en"
)
```

``` r
cr_age_pattern(
  agerate,
  cancer = "Trachea, Bronchus & Lung cancer",
  event_type = "mortality",
  area = 410000,
  lang = "en"
)
```

Selected core functions can also return structured values for checking
the numbers behind the rendered paragraph:

``` r
cr_overview(
  asr,
  cancer = "Breast cancer",
  event_type = "incidence",
  area = 410000,
  lang = "en",
  output = "both"
)
```

## Main functions

| Function | Description |
|----|----|
| `cr_coverage()` | Describe population coverage for cancer registration areas. |
| `cr_overview()` | Generate an overview paragraph for one cancer and one event type from an ASR table. |
| `cr_overview_total()` | Generate an overview paragraph for all cancers in one registration area. |
| `cr_topn()` | Describe the top N registry areas by a selected standardized rate. |
| `cr_age_pattern()` | Describe age-specific rate patterns and subgroup comparisons. |
| `cr_age_pattern_total()` | Describe overall age-specific patterns for all cancers. |
| `cr_spectrum()` | Summarize leading cancers in the cancer spectrum. |
| `cr_subsite()` | Summarize cancer counts by ICD-10 subsite or morphology group. |
| `cr_qua_summary()` | Summarize cancer registry data quality indicators. |

## Structured output

The default output of text generators remains a single character string.
For selected core functions, `output` can be used to inspect the values
used for template rendering:

- `output = "text"`: return the rendered paragraph.
- `output = "data"`: return a one-row data frame of template values.
- `output = "both"`: return a list with `text`, `data`, and `values`.

Currently this option is available for `cr_overview()`,
`cr_overview_total()`, and `cr_qua_summary()`.

## Template-based reporting

Narrative text is generated from templates and language dictionaries:

- Templates: `inst/templates/zh/` and `inst/templates/en/`
- Language dictionaries: `inst/lang/zh.yml` and `inst/lang/en.yml`
- R functions: data validation, filtering, calculation, formatting, and
  template rendering

This structure keeps the package easier to maintain when report wording
needs to be revised or new languages/templates are added.

## License

MIT © Qiong Chen

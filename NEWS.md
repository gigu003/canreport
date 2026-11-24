# canreport 0.1.0

## Improvements

- Added internal validation helpers for formatting digits, comparison
  thresholds, and output modes.
- Added structured output support to selected core functions:
  - `cr_overview()`
  - `cr_overview_total()`
  - `cr_qua_summary()`
- Added template quality tests for non-empty template strings, snake-case
  placeholders, and matching Chinese/English template structures.
- Expanded README coverage to include all exported text generator functions and
  structured output usage.

## Compatibility

- Existing default behavior is preserved: text generators continue to return a
  single character string by default.

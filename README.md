# volvelle

> Config-driven ROLLUP aggregation with an embeddable HTML drill-down widget

The name references the medieval rotating paper calculation instrument — nested analytical layers, each a distinct _face_ or facet of the data.

## Overview

**volvelle** provides a concise, declarative interface for building multi-level ROLLUP aggregations over tabular data and presenting them as interactive, drill-down HTML widgets in R Markdown reports and Shiny applications.

**Core workflow:**

1. Supply a `data.frame` and a YAML config file
2. Declare a column hierarchy, measure definitions, optional named filters, and optional derived (post-aggregation) fields in the config
3. `volvelle()` returns a structured list of ROLLUP-aggregated `data.table`s — one per filter facet plus one unfiltered (`"full"`)
4. `volvelle_widget()` wraps the result in a tabbed, self-contained HTML widget

## Installation

```r
# From GitHub (requires remotes or devtools)
remotes::install_github("lvegro/volvelle")
```

## Quick Start

```r
library(volvelle)

# 1. Point at the bundled demo config
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")

# 2. Run ROLLUP aggregation over the demo credit portfolio
result <- volvelle(credit_portfolio, cfg_path)
print(result)
#> <volvelle_result>
#>   Hierarchy : segment > sub_segment > counterparty
#>   Measures  : exposure, avg_pd, avg_lgd, n_obligors
#>   Facets    : full, performing, non_performing, large_exposure
#>   Derived   : expected_loss
#>   [full] 48 rows, 4 rollup levels
#>   ...

# 3. Render an interactive widget
volvelle_widget(result, title = "Credit Portfolio ROLLUP")
```

## Config File Format

```yaml
hierarchy:
  - segment       # outermost grouping level
  - sub_segment
  - counterparty  # finest level

measures:
  exposure:
    col: nominal_exposure
    fn: sum
    label: "Total Exposure"

  avg_pd:
    col: pd
    fn: weighted_mean
    weight: nominal_exposure
    label: "Weighted Avg PD"

  n_obligors:
    col: entity_id
    fn: n_distinct
    label: "# Obligors"

# Supported fn: sum, mean, weighted_mean, n, n_distinct, min, max

derived:
  expected_loss:
    expr: "avg_pd * avg_lgd * exposure"
    label: "Expected Loss"
    format: "%.0f"

filters:
  performing:
    col: status
    op: "=="
    val: "performing"

  large_exposure:
    col: nominal_exposure
    op: ">="
    val: 1000000

# Supported op: ==, !=, >, >=, <, <=, %in%
```

## Working with Results

```r
# Access individual facets as data.tables
result$full          # all data, ROLLUP aggregated
result$performing    # performing loans only, ROLLUP aggregated

# Grand total row (.rollup_depth = 0)
result$full[result$full$.rollup_depth == 0L, ]

# Convert to plain data.frame
df <- as.data.frame(result, facet = "performing")

# List available facets
facets(result)
#> [1] "full"           "performing"     "non_performing" "large_exposure"

# Inject ad-hoc filters at call time (no config change needed)
result2 <- volvelle(
  credit_portfolio, cfg_path,
  extra_filters = list(watch = list(col = "status", op = "==", val = "watch"))
)
```

## Widget Options

```r
# Light theme (default)
volvelle_widget(result, title = "My Report", height = "600px", theme = "light")

# Dark theme
volvelle_widget(result, theme = "dark")

# Save to standalone HTML
w <- volvelle_widget(result)
htmlwidgets::saveWidget(w, "report.html", selfcontained = TRUE)
```

## Package Structure

```
R/
  volvelle.R   — main exported function + S3 methods
  widget.R     — volvelle_widget()
  config.R     — parse_config(), validate_config()
  rollup.R     — .build_rollup(), .rollup_sets(), .build_agg_j()
  filters.R    — .apply_filters(), .build_filter_expr(), .build_all_facets()
  derived.R    — .apply_derived()
  utils.R      — shared helpers
inst/
  schema/volvelle_schema.json   — JSON Schema for config validation
  extdata/demo_config.yaml      — demo configuration
data/
  credit_portfolio.rda          — 500-row synthetic credit portfolio
```

## Dependencies

`data.table`, `dplyr`, `yaml`, `jsonvalidate`, `checkmate`, `reactable`, `htmlwidgets`, `htmltools`, `rlang`, `purrr`

## License

MIT

#' Render an interactive drill-down widget from a `volvelle_result`
#'
#' Produces a self-contained HTML widget with tabbed facets, each rendered as
#' an expandable `reactable` table. Rows are ordered by hierarchy level;
#' coarser-level rows are visually bolder. Grand-total rows are always visible.
#' Derived fields are displayed with their configured labels and sprintf formats.
#'
#' The widget is fully self-contained — no external CDN requests are made at
#' render time. `htmlwidgets::saveWidget()` produces a single portable HTML
#' file.
#'
#' @param x A `volvelle_result` object from `volvelle()`.
#' @param title Optional character string displayed as a heading above the widget.
#' @param height CSS height string for each table panel. Default `"600px"`.
#' @param theme One of `"light"` or `"dark"`. Default `"light"`.
#' @return An `htmlwidget` object, renderable in RStudio Viewer, R Markdown,
#'   Shiny, or a browser via `print()`.
#' @export
#' @examples
#' cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
#' result   <- volvelle(credit_portfolio, cfg_path)
#' w        <- volvelle_widget(result, title = "Credit Portfolio ROLLUP")
#' # print(w)  # opens in viewer
volvelle_widget <- function(x, title = NULL, height = "600px", theme = "light") {
  if (!inherits(x, "volvelle_result")) {
    rlang::abort("x must be a volvelle_result object", class = "volvelle_data_error")
  }
  checkmate::assert_string(height)
  checkmate::assert_choice(theme, c("light", "dark"))

  cfg    <- attr(x, "config")
  facets <- names(x)

  # Build one reactable per facet
  tables <- purrr::imap(x, function(dt, facet_name) {
    .build_facet_reactable(dt, cfg, facet_name, height, theme)
  })

  # Combine into tabbed HTML
  html <- .build_tabbed_widget(tables, facets, title, theme)

  # Wrap in htmlwidget via htmltools browsable
  htmltools::browsable(html)
}

# ---------------------------------------------------------------------------
# Internal: build a single reactable for one facet
# ---------------------------------------------------------------------------

.build_facet_reactable <- function(dt, cfg, facet_name, height, theme) {
  hierarchy <- cfg$hierarchy
  measures  <- cfg$measures
  derived   <- cfg$derived

  # Convert to data.frame for reactable
  df <- as.data.frame(dt)

  # Build column definitions
  col_defs <- list()

  # Hierarchy columns — left-aligned, styled by depth
  for (col in hierarchy) {
    col_defs[[col]] <- reactable::colDef(
      name  = col,
      align = "left",
      style = reactable::JS(
        "function(rowInfo) {
          var depth = rowInfo.values['.rollup_depth'];
          var weight = depth >= 2 ? 'bold' : (depth === 1 ? '600' : 'normal');
          var size   = depth >= 2 ? '1em' : (depth === 1 ? '0.95em' : '0.9em');
          return { fontWeight: weight, fontSize: size };
        }"
      )
    )
  }

  # Measure columns — right-aligned with thousand separators
  for (nm in names(measures)) {
    m     <- measures[[nm]]
    label <- .field_label(m, nm)

    col_defs[[nm]] <- reactable::colDef(
      name   = label,
      align  = "right",
      format = reactable::colFormat(separators = TRUE, digits = 4)
    )
  }

  # Derived field columns
  if (!is.null(derived)) {
    for (nm in names(derived)) {
      d     <- derived[[nm]]
      label <- .field_label(d, nm)
      fmt   <- d$format %||% "%.2f"

      col_defs[[nm]] <- reactable::colDef(
        name   = label,
        align  = "right",
        format = reactable::colFormat(separators = TRUE)
      )
    }
  }

  # Hide metadata columns
  col_defs[[".id"]]           <- reactable::colDef(show = FALSE)
  col_defs[[".rollup_depth"]] <- reactable::colDef(show = FALSE)

  # Sort: grand total (depth=0) first, then ascending depth (coarser before finer),
  # then by hierarchy columns for stable ordering within each level.
  sort_keys <- c(list(df$.rollup_depth), lapply(hierarchy, function(h) df[[h]]))
  df        <- df[do.call(order, sort_keys), ]

  # Theme-specific colours
  bg_colour  <- if (theme == "dark") "#1e1e2e" else "#ffffff"
  txt_colour <- if (theme == "dark") "#cdd6f4" else "#1a1a2e"
  hdr_bg     <- if (theme == "dark") "#313244" else "#f0f4f8"
  stripe_bg  <- if (theme == "dark") "#252535" else "#f8fafc"
  border_col <- if (theme == "dark") "#45475a" else "#e2e8f0"

  # Row styling based on .rollup_depth for visual hierarchy
  row_style <- reactable::JS(
    "function(rowInfo) {
      var d = rowInfo.values['.rollup_depth'];
      if (d === 0) return { fontWeight: 'bold', fontSize: '1.05em', borderTop: '2px solid #888' };
      if (d === 1) return { fontWeight: '700', paddingLeft: '0.5rem' };
      if (d === 2) return { fontWeight: '600', paddingLeft: '1.5rem' };
      return { paddingLeft: (d * 0.8) + 'rem' };
    }"
  )

  reactable::reactable(
    df,
    columns             = col_defs,
    rowStyle            = row_style,
    sortable            = TRUE,
    resizable           = TRUE,
    filterable          = FALSE,
    searchable          = TRUE,
    striped             = TRUE,
    highlight           = TRUE,
    bordered            = TRUE,
    height              = height,
    theme               = reactable::reactableTheme(
      color            = txt_colour,
      backgroundColor  = bg_colour,
      borderColor      = border_col,
      stripedColor     = stripe_bg,
      highlightColor   = if (theme == "dark") "#45475a" else "#e8f4fd",
      headerStyle      = list(backgroundColor = hdr_bg, fontWeight = "bold"),
      searchInputStyle = list(backgroundColor = bg_colour, color = txt_colour)
    ),
    defaultPageSize     = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions     = c(10, 25, 50, 100)
  )
}

# ---------------------------------------------------------------------------
# Internal: build the full tabbed HTML document
# ---------------------------------------------------------------------------

.build_tabbed_widget <- function(tables, facets, title, theme) {
  bg_colour  <- if (theme == "dark") "#1e1e2e" else "#f8fafc"
  txt_colour <- if (theme == "dark") "#cdd6f4" else "#1a1a2e"
  tab_active <- if (theme == "dark") "#89b4fa" else "#3b82f6"
  tab_bg     <- if (theme == "dark") "#313244" else "#e2e8f0"

  css <- sprintf(
    "
    body { margin: 0; padding: 0; font-family: -apple-system, BlinkMacSystemFont,
           'Segoe UI', Roboto, sans-serif; background: %s; color: %s; }
    .volvelle-container { max-width: 100%%; padding: 1rem; }
    .volvelle-title { font-size: 1.4rem; font-weight: 700; margin-bottom: 1rem; }
    .volvelle-tabs { display: flex; gap: 0.5rem; margin-bottom: 1rem; flex-wrap: wrap; }
    .volvelle-tab {
      padding: 0.4rem 1rem; border-radius: 0.4rem; border: none; cursor: pointer;
      background: %s; color: %s; font-size: 0.9rem; transition: background 0.2s;
    }
    .volvelle-tab.active { background: %s; color: #fff; font-weight: 600; }
    .volvelle-panel { display: none; }
    .volvelle-panel.active { display: block; }
    ",
    bg_colour, txt_colour, tab_bg, txt_colour, tab_active
  )

  title_html <- if (!is.null(title)) {
    htmltools::div(class = "volvelle-title", title)
  } else {
    htmltools::tagList()
  }

  # Tab buttons
  tab_buttons <- purrr::imap(facets, function(nm, idx) {
    htmltools::tags$button(
      class   = paste0("volvelle-tab", if (idx == 1) " active" else ""),
      onclick = sprintf(
        "volvelleShowTab(this, 'volvelle-panel-%d')", idx
      ),
      nm
    )
  })

  # Panels
  panels <- purrr::imap(facets, function(nm, idx) {
    htmltools::div(
      id    = sprintf("volvelle-panel-%d", idx),
      class = paste0("volvelle-panel", if (idx == 1) " active" else ""),
      tables[[nm]]
    )
  })

  js <- "
  function volvelleShowTab(btn, panelId) {
    document.querySelectorAll('.volvelle-tab').forEach(function(b) {
      b.classList.remove('active');
    });
    document.querySelectorAll('.volvelle-panel').forEach(function(p) {
      p.classList.remove('active');
    });
    btn.classList.add('active');
    document.getElementById(panelId).classList.add('active');
  }
  "

  htmltools::tagList(
    htmltools::tags$style(css),
    htmltools::tags$script(js),
    htmltools::div(
      class = "volvelle-container",
      title_html,
      htmltools::div(class = "volvelle-tabs", tab_buttons),
      panels
    )
  )
}

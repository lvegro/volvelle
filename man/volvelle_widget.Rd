\name{volvelle_widget}
\alias{volvelle_widget}
\title{Render an interactive drill-down widget from a \code{volvelle_result}}
\usage{
volvelle_widget(x, title = NULL, height = "600px", theme = "light")
}
\arguments{
\item{x}{A \code{volvelle_result} object from \code{volvelle()}.}

\item{title}{Optional character string displayed as a heading above the widget.}

\item{height}{CSS height string for each table panel. Default \code{"600px"}.}

\item{theme}{One of \code{"light"} or \code{"dark"}. Default \code{"light"}.}
}
\value{
An \code{htmlwidget} object, renderable in RStudio Viewer, R Markdown,
Shiny, or a browser via \code{print()}.
}
\description{
Produces a self-contained HTML widget with tabbed facets, each rendered as
an expandable \code{reactable} table. Rows are ordered by hierarchy level;
coarser-level rows are visually bolder. Grand-total rows are always visible.
Derived fields are displayed with their configured labels and sprintf formats.

The widget is fully self-contained — no external CDN requests are made at
render time. \code{htmlwidgets::saveWidget()} produces a single portable HTML
file.
}
\examples{
cfg_path <- system.file("extdata", "demo_config.yaml", package = "volvelle")
result   <- volvelle(credit_portfolio, cfg_path)
w        <- volvelle_widget(result, title = "Credit Portfolio ROLLUP")
# print(w)  # opens in viewer / browser
}

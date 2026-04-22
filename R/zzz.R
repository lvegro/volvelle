.onLoad <- function(libname, pkgname) {
  # Suppress data.table startup message in package context
  data.table::setDTthreads(0L)
}

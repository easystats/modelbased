.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv(OMP_THREAD_LIMIT = 2)
}

requiet <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

# load all hard dependencies
requiet("bayestestR")
requiet("datawizard")
requiet("effectsize")
requiet("insight")
requiet("parameters")
requiet("performance")

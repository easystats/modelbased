library(testthat)
library(modelbased)

osx <- tryCatch(
  {
    si <- Sys.info()
    if (!is.null(si["sysname"])) {
      si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
    } else {
      FALSE
    }
  },
  error = function(e) {
    FALSE
  }
)

if (!osx || (osx && getRversion() >= "4.0.0")) {
  test_check("modelbased")
}

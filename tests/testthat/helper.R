requiet <- function(package) {
  testthat::skip_if_not_installed(package)
  suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )
}

# load all hard deps
library(bayestestR)
library(datawizard)
library(effectsize)
library(insight)
library(parameters)
library(performance)

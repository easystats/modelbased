#' @docType data
#' @title Sample data set
#' @name fish
#' @keywords data
#'
#' @description A sample data set, used in tests and some examples. Useful for
#' demonstrating count models (with or without zero-inflation component). It
#' consists of nine variables from 250 observations.
NULL


#' @docType data
#' @title Sample dataset from the EFC Survey
#' @name efc
#' @keywords data
#'
#' @description Selected variables from the EUROFAMCARE survey. Useful when
#' testing on "real-life" data sets, including random missing values. This
#' data set also has value and variable label attributes.
NULL


#' @docType data
#' @title More puppy therapy data
#' @name puppy_love
#' @keywords data
#'
#' @description Fictitious data related to whether puppy therapy works when you
#' adjust for a person’s love of puppies, taken from the `{discovr}` package
#' (Field 2025)
#'
#' @details Following variables are included in the dataset:
#' - `id``: Participant id
#' - `dose`: Treatment group to which the participant was randomly assigned (No
#'   puppies (control), 15 minutes of puppy therapy, 30 minutes of puppy
#'   therapy)
#' - `happiness`: Self-reported happiness from 0 (as unhappy as I can possibly
#'   imagine being) to 10 (as happy as I can possibly imagine being)
#' - `puppy_love`: Self-reported love of puppies from 0 (I am a weird person who
#'   hates puppies, please be deeply suspicious of me) to 7 (puppies are the
#'   best thing ever, one day I might marry one)
#'
#' For further details, see `?discovr::puppy_love`.
#'
#' @references Field, A. P. (2025). Discovering statistics using R and RStudio
#' (2nd ed.). London: Sage.
NULL


#' @docType data
#' @title Sample dataset from a course about analysis of factorial designs
#' @name coffee_data
#' @keywords data
#'
#' @description A sample data set from a course about the analysis of factorial
#' designs, by Mattan S. Ben-Shachar. See following link for more information:
#' https://github.com/mattansb/Analysis-of-Factorial-Designs-foR-Psychologists
#'
#' The data consists of five variables from 120 observations:
#'
#' - `ID`: A unique identifier for each participant
#' - `sex`: The participant's sex
#' - `time`: The time of day the participant was tested (morning, noon, or afternoon)
#' - `coffee`: Group indicator, whether participant drank coffee or not
#'   ("`coffee"` or `"control"`).
#' - `alertness`: The participant's alertness score.
NULL

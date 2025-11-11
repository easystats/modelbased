# Converting modelbased-objects into raw data frames

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
for **modelbased** objects. Can be used to return a "raw" data frame
without attributes and with standardized column names. By default, the
original column names are preserved, to avoid unexpected changes, but
this can be changed with the `preserve_names` argument.

## Usage

``` r
# S3 method for class 'estimate_contrasts'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  use_responsename = FALSE,
  preserve_names = TRUE
)
```

## Arguments

- x:

  An object returned by the different `estimate_*()` functions.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- ...:

  Arguments passed to the `data.frame` method of
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- stringsAsFactors:

  logical: should the character vector be converted to a factor?

- use_responsename:

  Logical, if `TRUE`, the response variable name is used as column name
  for the estimate column (if available). If `FALSE` (default), the
  column is named `"Coefficient"`.

- preserve_names:

  Logical, if `TRUE` (default), the original column names are preserved.
  If `FALSE`, the estimate column is renamed to either the response name
  (if `use_responsename = TRUE`) or to `"Coefficient"`.

## Value

A data frame.

## Examples

``` r
model <- lm(Petal.Length ~ Species, data = iris)
out <- estimate_means(model, "Species")

# default
print(out)
#> Estimated Marginal Means
#> 
#> Species    | Mean |   SE |       95% CI | t(147)
#> ------------------------------------------------
#> setosa     | 1.46 | 0.06 | [1.34, 1.58] |  24.02
#> versicolor | 4.26 | 0.06 | [4.14, 4.38] |  70.00
#> virginica  | 5.55 | 0.06 | [5.43, 5.67] |  91.23
#> 
#> Variable predicted: Petal.Length
#> Predictors modulated: Species
#> 

as.data.frame(out)
#>      Species  Mean         SE   CI_low  CI_high        t  df
#> 1     setosa 1.462 0.06085848 1.341729 1.582271 24.02294 147
#> 2 versicolor 4.260 0.06085849 4.139729 4.380271 69.99846 147
#> 3  virginica 5.552 0.06085848 5.431729 5.672271 91.22804 147

as.data.frame(out, preserve_names = FALSE)
#>      Species Coefficient         SE   CI_low  CI_high        t  df
#> 1     setosa       1.462 0.06085848 1.341729 1.582271 24.02294 147
#> 2 versicolor       4.260 0.06085849 4.139729 4.380271 69.99846 147
#> 3  virginica       5.552 0.06085848 5.431729 5.672271 91.22804 147

as.data.frame(out, preserve_names = FALSE, use_responsename = TRUE)
#>      Species Petal.Length         SE   CI_low  CI_high        t  df
#> 1     setosa        1.462 0.06085848 1.341729 1.582271 24.02294 147
#> 2 versicolor        4.260 0.06085849 4.139729 4.380271 69.99846 147
#> 3  virginica        5.552 0.06085848 5.431729 5.672271 91.22804 147
```

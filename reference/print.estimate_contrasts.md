# Printing modelbased-objects

[`print()`](https://rdrr.io/r/base/print.html) method for **modelbased**
objects. Can be used to tweak the output of tables.

## Usage

``` r
# S3 method for class 'estimate_contrasts'
display(
  object,
  select = NULL,
  include_grid = NULL,
  full_labels = NULL,
  format = "markdown",
  ...
)

# S3 method for class 'estimate_contrasts'
format(
  x,
  format = NULL,
  select = getOption("modelbased_select", NULL),
  include_grid = getOption("modelbased_include_grid", FALSE),
  ...
)

# S3 method for class 'estimate_contrasts'
print(x, select = NULL, include_grid = NULL, full_labels = NULL, ...)
```

## Arguments

- select:

  Determines which columns are printed and the table layout. There are
  two options for this argument:

  - **A string expression with layout pattern**

    `select` is a string with "tokens" enclosed in braces. These tokens
    will be replaced by their associated columns, where the selected
    columns will be collapsed into one column. Following tokens are
    replaced by the related coefficients or statistics: `{estimate}`,
    `{se}`, `{ci}` (or `{ci_low}` and `{ci_high}`), `{p}`, `{pd}` and
    `{stars}`. The token `{ci}` will be replaced by
    `{ci_low}, {ci_high}`. Example:
    `select = "{estimate}{stars} ({ci})"`

    It is possible to create multiple columns as well. A `|` separates
    values into new cells/columns. Example:
    `select = "{estimate} ({ci})|{p}"`.

  - **A string indicating a pre-defined layout**

    `select` can be one of the following string values, to create one of
    the following pre-defined column layouts:

    - `"minimal"`: Estimates, confidence intervals and numeric p-values,
      in two columns. This is equivalent to
      `select = "{estimate} ({ci})|{p}"`.

    - `"short"`: Estimate, standard errors and numeric p-values, in two
      columns. This is equivalent to `select = "{estimate} ({se})|{p}"`.

    - `"ci"`: Estimates and confidence intervals, no asterisks for
      p-values. This is equivalent to `select = "{estimate} ({ci})"`.

    - `"se"`: Estimates and standard errors, no asterisks for p-values.
      This is equivalent to `select = "{estimate} ({se})"`.

    - `"ci_p"`: Estimates, confidence intervals and asterisks for
      p-values. This is equivalent to
      `select = "{estimate}{stars} ({ci})"`.

    - `"se_p"`: Estimates, standard errors and asterisks for p-values.
      This is equivalent to `select = "{estimate}{stars} ({se})"`..

  Using `select` to define columns will re-order columns and remove all
  columns related to uncertainty (standard errors, confidence
  intervals), test statistics, and p-values (and similar, like `pd` or
  `BF` for Bayesian models), because these are assumed to be included or
  intentionally excluded when using `select`. The new column order will
  be: Parameter columns first, followed by the "glue" columns, followed
  by all remaining columns. If further columns should also be placed
  first, add those as `focal_terms` attributes to `x`. I.e., following
  columns are considers as "parameter columns" and placed first:
  `c(easystats_columns("parameter"), attributes(x)$focal_terms)`.

  **Note:** glue-like syntax is still experimental in the case of more
  complex models (like mixed models) and may not return expected
  results.

- include_grid:

  Logical, if `TRUE`, the data grid is included in the table output.
  Only applies to prediction-functions like
  [`estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md)
  or
  [`estimate_link()`](https://easystats.github.io/modelbased/reference/estimate_expectation.md).
  Default is `NULL`, which will set the value based on
  `options(modelbased_include_grid)`, and use `FALSE` is no option is
  set.

- full_labels:

  Logical, if `TRUE` (default), all labels for focal terms are shown. If
  `FALSE`, redundant (duplicated) labels are removed from rows. Default
  is `NULL`, which will set the value based on
  `options(modelbased_full_labels)`, and use `TRUE` is no option is set.

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "html"` create a HTML table using the *gt* package.
  `format = "tt"` creates a `tinytable` object, which is either printed
  as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

- ...:

  Arguments passed to
  [`insight::format_table()`](https://easystats.github.io/insight/reference/format_table.html)
  or
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

- x, object:

  An object returned by the different `estimate_*()` functions.

## Value

Invisibly returns `x`.

## Note

Use
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
and
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
to create tables in HTML or markdown format, respectively.

## Global Options to Customize Tables when Printing

Columns and table layout can be customized using
[`options()`](https://rdrr.io/r/base/options.html):

- `modelbased_select`: `options(modelbased_select = <string>)` will set
  a default value for the `select` argument and can be used to define a
  custom default layout for printing.

- `modelbased_include_grid`: `options(modelbased_include_grid = TRUE)`
  will set a default value for the `include_grid` argument and can be
  used to include data grids in the output by default or not.

- `modelbased_full_labels`: `options(modelbased_full_labels = FALSE)`
  will remove redundant (duplicated) labels from rows.

- `easystats_display_format`:
  `options(easystats_display_format = <value>)` will set the default
  format for the
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods. Can be one of `"markdown"`, `"html"`, or `"tt"`.

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

# smaller set of columns
print(out, select = "minimal")
#> Estimated Marginal Means
#> 
#> Species    |         Mean (CI)
#> ------------------------------
#> setosa     | 1.46 (1.34, 1.58)
#> versicolor | 4.26 (4.14, 4.38)
#> virginica  | 5.55 (5.43, 5.67)
#> 
#> Variable predicted: Petal.Length
#> Predictors modulated: Species
#> 

# remove redundant labels
data(efc, package = "modelbased")
efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex"))
levels(efc$c172code) <- c("low", "mid", "high")
fit <- lm(neg_c_7 ~ c161sex * c172code * e16sex, data = efc)
out <- estimate_means(fit, c("c161sex", "c172code", "e16sex"))
print(out, full_labels = FALSE, select = "{estimate} ({se})")
#> Estimated Marginal Means
#> 
#> c161sex | c172code | e16sex |    Mean (SE)
#> ------------------------------------------
#> Male    | low      | male   |  9.47 (1.00)
#> Female  |          |        | 12.13 (0.48)
#> Male    | mid      |        | 12.16 (0.68)
#> Female  |          |        | 12.48 (0.35)
#> Male    | high     |        | 12.31 (1.07)
#> Female  |          |        | 12.37 (0.71)
#> Male    | low      | female | 11.92 (0.76)
#> Female  |          |        | 12.11 (0.46)
#> Male    | mid      |        | 10.93 (0.43)
#> Female  |          |        | 11.57 (0.24)
#> Male    | high     |        | 11.42 (0.67)
#> Female  |          |        | 12.74 (0.44)
#> 
#> Variable predicted: neg_c_7
#> Predictors modulated: c161sex, c172code, e16sex
#> 
```

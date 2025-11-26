# Global options from the modelbased package

Global options from the modelbased package

## Global options to set defaults for function arguments

**For calculating marginal means**

- `options(modelbased_backend = <string>)` will set a default value for
  the `backend` argument and can be used to set the package used by
  default to calculate marginal means. Can be `"marginaleffects"` or
  `"emmeans"`.

- `options(modelbased_estimate = <string>)` will set a default value for
  the `estimate` argument, which modulates the type of target population
  predictions refer to.

- `options(modelbased_integer = <value>)` will set the minimum number of
  unique values in an integer predictor to treat that predictor as a
  "discrete integer" or as continuous. If the integer has more than
  `modelbased_integer` unique values, it is treated as continuous. Set
  to `TRUE` to always treat integer predictors as continuous.

**For printing**

- `options(modelbased_select = <string>)` will set a default value for
  the `select` argument and can be used to define a custom default
  layout for printing.

- `options(modelbased_include_grid = TRUE)` will set a default value for
  the `include_grid` argument and can be used to include data grids in
  the output by default or not.

- `options(modelbased_full_labels = FALSE)` will remove redundant
  (duplicated) labels from rows.

- `options(easystats_display_format = <value>)` will set the default
  format for the
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods. Can be one of `"markdown"`, `"html"`, or `"tt"`. See
  [`display.estimate_contrasts()`](https://easystats.github.io/modelbased/reference/print.estimate_contrasts.md)
  for details.

**For plotting**

- `options(modelbased_join_dots = <logical>)` will set a default value
  for the `join_dots`.

- `options(modelbased_numeric_as_discrete = <number>)` will set a
  default value for the `modelbased_numeric_as_discrete` argument. Can
  also be `FALSE`.

- `options(modelbased_ribbon_alpha = <number>)` will set a default value
  for the `alpha` argument of the `ribbon` geom. Should be a number
  between `0` and `1`.

- `options(modelbased_tinyplot_dodge = <number>)` will set a default
  value for the `dodge` argument (spacing between geoms) when using
  [`tinyplot::plt()`](https://grantmcdermott.com/tinyplot/man/tinyplot.html).
  Should be a number between `0` and `1`.

# Collapse raw data by random effect groups

This function extracts the raw data points (i.e. the data that was used
to fit the model) and "averages" (i.e. "collapses") the response
variable over the levels of the grouping factor given in `collapse_by`.
Only works with mixed models.

## Usage

``` r
collapse_by_group(grid, model, collapse_by = NULL, residuals = FALSE)
```

## Arguments

- grid:

  A data frame representing the data grid, or an object of class
  `estimate_means` or `estimate_predicted`, as returned by the different
  `estimate_*()` functions.

- model:

  The model for which to compute partial residuals. The data grid `grid`
  should match to predictors in the model.

- collapse_by:

  Name of the (random effects) grouping factor. Data is collapsed by the
  levels of this factor.

- residuals:

  Logical, if `TRUE`, collapsed partial residuals instead of raw data by
  the levels of the grouping factor.

## Value

A data frame with raw data points, averaged over the levels of the given
grouping factor from the random effects. The group level of the random
effect is saved in the column `"random"`.

## Examples

``` r
data(efc, package = "modelbased")
efc$e15relat <- as.factor(efc$e15relat)
efc$c161sex <- as.factor(efc$c161sex)
levels(efc$c161sex) <- c("male", "female")
model <- lme4::lmer(neg_c_7 ~ c161sex + (1 | e15relat), data = efc)
me <- estimate_means(model, "c161sex")
head(efc)
#>   c12hour e15relat e16sex e17age e42dep c82cop1 c83cop2 c84cop3 c85cop4 c86cop5
#> 1      16        2      2     83      3       3       2       2       2       1
#> 2     148        2      2     88      3       3       3       3       3       4
#> 3      70        1      2     82      3       2       2       1       4       1
#> 4     168        1      2     67      4       4       1       3       1       1
#> 5     168        2      2     84      4       3       2       1       2       2
#> 6      16        2      2     85      4       2       2       3       3       3
#>   c87cop6 c88cop7 c89cop8 c90cop9 c160age c161sex c172code c175empl barthtot
#> 1       1       2       3       3      56  female        2        1       75
#> 2       1       3       2       2      54  female        2        1       75
#> 3       1       1       4       3      80    male        1        0       35
#> 4       1       1       2       4      69    male        2        0        0
#> 5       2       1       4       4      47  female        2        0       25
#> 6       2       2       1       1      56    male        2        1       60
#>   neg_c_7 pos_v_4 quol_5 resttotn tot_sc_e n4pstu nur_pst            grp negc7d
#> 1      12      12     14        0        4      0      NA          child      1
#> 2      20      11     10        4        0      0      NA          child      1
#> 3      11      13      7        0        1      2       2 spouse/partner      0
#> 4      10      15     12        2        0      3       3 spouse/partner      0
#> 5      12      15     19        2        1      2       2          child      1
#> 6      19       9      8        1        3      2       2          child      1
collapse_by_group(me, model, "e15relat")
#>    c161sex e15relat random   neg_c_7 group_col
#> 1     male        1      1 12.297872         1
#> 2   female        1      1 13.347107         1
#> 3     male        2      2 11.585586         1
#> 4   female        2      2 12.118310         1
#> 5     male        3      3 12.166667         1
#> 6   female        3      3 10.545455         1
#> 7     male        4      4 10.750000         1
#> 8   female        4      4 11.726027         1
#> 9     male        5      5 11.333333         1
#> 10  female        5      5 10.235294         1
#> 11    male        6      6  8.200000         1
#> 12  female        6      6  9.235294         1
#> 13    male        7      7 13.000000         1
#> 14  female        7      7 10.400000         1
#> 15    male        8      8  9.666667         1
#> 16  female        8      8 10.955882         1
```

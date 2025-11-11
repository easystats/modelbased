# Find zero-crossings and inversion points

Find zero crossings of a vector, i.e., indices when the numeric variable
crosses 0. It is useful for finding the points where a function changes
by looking at the zero crossings of its derivative.

## Usage

``` r
zero_crossings(x)

find_inversions(x)
```

## Arguments

- x:

  A numeric vector.

## Value

Vector of zero crossings or points of inversion.

## See also

Based on the `uniroot.all` function from the rootSolve package.

## Examples

``` r
x <- sin(seq(0, 4 * pi, length.out = 100))
# plot(x, type = "b")

modelbased::zero_crossings(x)
#> [1]  1.00000 25.74975 50.50000 75.25025
modelbased::find_inversions(x)
#> [1] 12.87478 37.62484 62.37516 87.12522
```

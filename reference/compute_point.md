# Compute a derived point of an identity level

The point
[`add_point()`](https://animovement.dev/animetric/reference/add_point.md)
appends, on its own: one row per position of every other identity
variable, derived from the members of the collapsed one.

## Usage

``` r
compute_point(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL
)
```

## Arguments

- data:

  An anipoint with Cartesian coordinates.

- across:

  Identity variables to collapse — the dimensions the new point is
  derived over. Required when the frame declares more than one identity
  variable, since their order is not a hierarchy and there is no finest
  one to assume; with a single identity variable, that one is the
  default. Collapsing every level gives a single point per position.

- method:

  How each coordinate is derived from the members' values:

  `"centroid"`

  :   The mean.

  `"median"`

  :   The median, per axis: robust to a single stray point.

  `"weighted"`

  :   The mean weighted by `confidence`, so poorly tracked points count
      for less. Needs a `confidence` column.

  a function

  :   Applied to each axis's values, with missing values removed,
      returning one number; e.g. `\(x) mean(x, trim = 0.1)`.

  Missing values are ignored; a moment where every member is missing
  gives `NA`.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed. A midpoint is the centroid of two
  members: `include = c("ear_l", "ear_r")`.

- name:

  Name for the new member. Defaults to `"centroid"`, `"median"` or
  `"weighted_centroid"` after `method`; required when `method` is a
  function.

## Value

An anipoint containing only the new member. Its `confidence`, if the
frame has one, is `NA`.

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 3)

# The centroid of each animal's keypoints
compute_point(af, across = "keypoint")
#> # Individuals: 1, 2
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 centroid       1     1     1  0.364   0.506         NA
#>  2          1 centroid       1     1     2  1.29    1.23          NA
#>  3          1 centroid       1     1     3 -0.0271  0.885         NA
#>  4          1 centroid       1     1     4 -0.183  -0.748         NA
#>  5          1 centroid       1     1     5 -1.10   -0.472         NA
#>  6          1 centroid       1     1     6 -0.259  -0.845         NA
#>  7          1 centroid       1     1     7  0.149  -0.322         NA
#>  8          1 centroid       1     1     8  0.163  -0.325         NA
#>  9          1 centroid       1     1     9 -1.10   -0.678         NA
#> 10          1 centroid       1     1    10 -0.199  -0.249         NA
#> # ℹ 30 more rows

# Their median
compute_point(af, across = "keypoint", method = "median")
#> # Individuals: 1, 2
#> # Keypoints:   median
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 median         1     1     1  0.413   0.189          NA
#>  2          1 median         1     1     2  0.948   1.22           NA
#>  3          1 median         1     1     3 -0.174   1.05           NA
#>  4          1 median         1     1     4  0.136  -0.869          NA
#>  5          1 median         1     1     5 -0.946  -0.618          NA
#>  6          1 median         1     1     6 -0.421  -0.557          NA
#>  7          1 median         1     1     7 -0.0501  0.0584         NA
#>  8          1 median         1     1     8  0.417   0.222          NA
#>  9          1 median         1     1     9 -1.14   -0.962          NA
#> 10          1 median         1     1    10 -0.310   0.0520         NA
#> # ℹ 30 more rows
```

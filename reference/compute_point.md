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
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.617   0.670          NA
#>  2          1 centroid       1     1     2 -0.396  -0.283          NA
#>  3          1 centroid       1     1     3  0.542  -0.309          NA
#>  4          1 centroid       1     1     4  0.0862 -0.518          NA
#>  5          1 centroid       1     1     5  0.239   0.0110         NA
#>  6          1 centroid       1     1     6  0.234  -0.0932         NA
#>  7          1 centroid       1     1     7 -0.487  -0.637          NA
#>  8          1 centroid       1     1     8 -0.0600 -0.0888         NA
#>  9          1 centroid       1     1     9  0.570  -1.03           NA
#> 10          1 centroid       1     1    10 -0.912  -0.538          NA
#> # ℹ 30 more rows

# Their median
compute_point(af, across = "keypoint", method = "median")
#> # Individuals: 1, 2
#> # Keypoints:   median
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>      <dbl>
#>  1          1 median         1     1     1 -0.545  0.642          NA
#>  2          1 median         1     1     2 -0.975 -0.386          NA
#>  3          1 median         1     1     3 -0.530 -0.242          NA
#>  4          1 median         1     1     4 -0.452 -0.834          NA
#>  5          1 median         1     1     5 -0.202  0.398          NA
#>  6          1 median         1     1     6 -0.365  0.470          NA
#>  7          1 median         1     1     7 -0.869 -0.395          NA
#>  8          1 median         1     1     8 -0.156 -0.0704         NA
#>  9          1 median         1     1     9  0.542 -0.613          NA
#> 10          1 median         1     1    10 -0.954 -0.337          NA
#> # ℹ 30 more rows
```

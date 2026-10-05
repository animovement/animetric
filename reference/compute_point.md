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
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.669   0.229           NA
#>  2          1 centroid       1     1     2  1.24    0.725           NA
#>  3          1 centroid       1     1     3 -0.536   0.277           NA
#>  4          1 centroid       1     1     4 -0.164   0.762           NA
#>  5          1 centroid       1     1     5 -0.285   0.0518          NA
#>  6          1 centroid       1     1     6 -0.405   0.0274          NA
#>  7          1 centroid       1     1     7  0.277  -0.287           NA
#>  8          1 centroid       1     1     8  0.0476  0.121           NA
#>  9          1 centroid       1     1     9 -0.940   0.236           NA
#> 10          1 centroid       1     1    10 -0.121  -0.00687         NA
#> # ℹ 30 more rows

# Their median
compute_point(af, across = "keypoint", method = "median")
#> # Individuals: 1, 2
#> # Keypoints:   median
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 median         1     1     1 -0.731   0.598         NA
#>  2          1 median         1     1     2  0.575   0.480         NA
#>  3          1 median         1     1     3 -0.206   0.157         NA
#>  4          1 median         1     1     4 -0.0801  0.959         NA
#>  5          1 median         1     1     5 -0.0323  0.153         NA
#>  6          1 median         1     1     6 -0.719   0.326         NA
#>  7          1 median         1     1     7  0.372  -0.796         NA
#>  8          1 median         1     1     8  0.355   0.245         NA
#>  9          1 median         1     1     9 -0.984   0.470         NA
#> 10          1 median         1     1    10 -0.150   0.535         NA
#> # ℹ 30 more rows
```

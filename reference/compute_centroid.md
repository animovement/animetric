# Compute centroid from keypoints

Calculates the mean position of selected keypoints at each time point.
The centroid is computed for each combination of grouping variables
(individual, time, trial/session if present).

## Usage

``` r
compute_centroid(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
)
```

## Arguments

- data:

  An aniframe with Cartesian coordinates (x, y, and/or z columns).

- include_keypoints:

  Character vector of keypoints to include in centroid calculation. If
  NULL (default), all keypoints are used unless `exclude_keypoints` is
  specified. Mutually exclusive with `exclude_keypoints`.

- exclude_keypoints:

  Character vector of keypoints to exclude from centroid calculation. If
  NULL (default), no keypoints are excluded. Mutually exclusive with
  `include_keypoints`.

- centroid_name:

  Name for the new centroid keypoint. Default is "centroid".

## Value

An aniframe containing only the centroid keypoint. Coordinate values are
the mean of selected keypoints (with NA values removed). Confidence is
set to NA. Missing coordinate dimensions return NA.

## Examples

``` r
af <- aniframe::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 3)
compute_centroid(af)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x        y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>    <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.287   -0.00193         NA
#>  2          1 centroid       1     1     2 -0.00695  0.0715          NA
#>  3          1 centroid       1     1     3 -1.26    -0.301           NA
#>  4          1 centroid       1     1     4  0.160    0.292           NA
#>  5          1 centroid       1     1     5  0.985    0.723           NA
#>  6          1 centroid       1     1     6  1.27    -0.430           NA
#>  7          1 centroid       1     1     7 -0.904   -0.640           NA
#>  8          1 centroid       1     1     8  0.105    0.457           NA
#>  9          1 centroid       1     1     9 -0.318    1.03            NA
#> 10          1 centroid       1     1    10 -1.24    -1.03            NA
#> 11          1 centroid       1     1    11  0.415   -0.346           NA
#> 12          1 centroid       1     1    12  0.187   -0.0360          NA
#> 13          1 centroid       1     1    13  0.701    0.852           NA
#> 14          1 centroid       1     1    14  0.00391 -0.0855          NA
#> 15          1 centroid       1     1    15  0.218    0.842           NA
#> 16          1 centroid       1     1    16 -0.482   -0.802           NA
#> 17          1 centroid       1     1    17 -1.57     1.15            NA
#> 18          1 centroid       1     1    18  0.784   -0.244           NA
#> 19          1 centroid       1     1    19 -0.0439   1.06            NA
#> 20          1 centroid       1     1    20  0.122    0.483           NA

# A centroid from a subset of keypoints
compute_centroid(af, include_keypoints = c("head", "neck"))
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x       y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.466    0.484          NA
#>  2          1 centroid       1     1     2  0.309   -0.528          NA
#>  3          1 centroid       1     1     3 -1.87    -0.932          NA
#>  4          1 centroid       1     1     4  0.366    0.0538         NA
#>  5          1 centroid       1     1     5  1.26     0.566          NA
#>  6          1 centroid       1     1     6  0.525   -0.408          NA
#>  7          1 centroid       1     1     7 -1.38    -0.322          NA
#>  8          1 centroid       1     1     8 -0.132    0.839          NA
#>  9          1 centroid       1     1     9 -0.535    0.436          NA
#> 10          1 centroid       1     1    10 -0.898   -1.03           NA
#> 11          1 centroid       1     1    11  0.191    0.0542         NA
#> 12          1 centroid       1     1    12  0.403    0.784          NA
#> 13          1 centroid       1     1    13  1.15     0.515          NA
#> 14          1 centroid       1     1    14 -0.00372 -0.405          NA
#> 15          1 centroid       1     1    15  0.312    0.267          NA
#> 16          1 centroid       1     1    16 -0.999   -1.13           NA
#> 17          1 centroid       1     1    17 -1.22     0.439          NA
#> 18          1 centroid       1     1    18 -0.166   -0.897          NA
#> 19          1 centroid       1     1    19  0.115    1.03           NA
#> 20          1 centroid       1     1    20  0.0766   0.163          NA
```

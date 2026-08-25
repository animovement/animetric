# Summarize keypoint data

Creates summary statistics across multiple keypoints at each time point.
Currently supports computing centroids from selected keypoints. Future
functionality will include polygonal summaries.

## Usage

``` r
summarise_keypoints(
  data,
  keypoints = "all",
  name = "centroid",
  add_area = FALSE
)
```

## Arguments

- data:

  An aniframe containing keypoint data.

- keypoints:

  Character vector of keypoint names to summarize, or "all" to use all
  keypoints in the data. Default is "all".

- name:

  Character string for the name of the new summary keypoint. Default is
  "centroid".

- add_area:

  Logical indicating whether to compute area (not yet implemented).
  Default is FALSE.

## Value

An aniframe with the original data plus the new summary keypoint.

## Examples

``` r
af <- aniframe::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 3)
summarise_keypoints(af)
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right, centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1  0.698   0.0639      0.741
#>  2          1 head           1     1     2 -0.864  -0.919       0.469
#>  3          1 head           1     1     3 -1.09    0.901       0.878
#>  4          1 head           1     1     4 -0.0371 -0.798       0.704
#>  5          1 head           1     1     5  0.810   0.668       0.846
#>  6          1 head           1     1     6 -0.499   0.155       0.848
#>  7          1 head           1     1     7  0.948   0.129       0.839
#>  8          1 head           1     1     8 -0.174  -1.53        0.557
#>  9          1 head           1     1     9 -1.11    0.202       0.758
#> 10          1 head           1     1    10 -0.946  -0.718       0.929
#> # ℹ 70 more rows
```

# Summarize keypoint data

Renamed
[`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md).
The old name said `summarise_`, which in this package means collapsing a
frame to summary rows — but this appends rows and returns the input as
well. It also named the keypoint level, which is only one of the levels
a frame may be summarised across.

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
  keypoints in the data.

- name:

  Character string for the name of the new summary keypoint.

- add_area:

  Ignored. Area was never implemented; it will arrive as its own
  function rather than a flag on this one.

## Value

An aniframe with the original data plus the new summary keypoint.

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 3)
add_centroid(af, across = "keypoint")
#> # Individuals: 1
#> # Keypoints:   head, neck, shoulder_right, centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1  0.882 -1.62       0.710
#>  2          1 head           1     1     2 -0.537  0.496      0.734
#>  3          1 head           1     1     3  1.29   1.30       0.580
#>  4          1 head           1     1     4  0.588 -1.62       0.793
#>  5          1 head           1     1     5 -1.31  -1.25       0.569
#>  6          1 head           1     1     6  0.317  1.58       0.817
#>  7          1 head           1     1     7  1.19   1.25       0.747
#>  8          1 head           1     1     8  0.913 -0.209      0.958
#>  9          1 head           1     1     9 -0.787 -0.639      0.931
#> 10          1 head           1     1    10 -0.411  0.399      0.889
#> # ℹ 70 more rows
```

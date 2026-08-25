# Summarise an aniframe

Calculate summary statistics for aniframe data by dispatching to
specialised summary functions.

## Usage

``` r
summarise_aniframe(
  data,
  type = c("kinematics", "tortuosity"),
  measures = c("median_mad", "mean_sd")
)

summarize_aniframe(
  data,
  type = c("kinematics", "tortuosity"),
  measures = c("median_mad", "mean_sd")
)
```

## Arguments

- data:

  A kinematics aniframe (output of
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md))

- type:

  Character vector of summary types. Options are `"kinematics"` and
  `"tortuosity"`. Default is both.

- measures:

  Measures of central tendency and dispersion for kinematics. Options
  are `"median_mad"` (default) and `"mean_sd"`.

## Value

A summarised data frame with one row per group.

## See also

[`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md),
[`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)

## Examples

``` r
kin <- calculate_kinematics(
  aniframe::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
summarise_aniframe(kin)
#> # A tibble: 1 × 22
#>   individual keypoint session trial median_speed mad_speed median_acceleration
#>        <int> <fct>      <int> <int>        <dbl>     <dbl>               <dbl>
#> 1          1 centroid       1     1        0.681     0.627              -0.123
#> # ℹ 15 more variables: mad_acceleration <dbl>, median_angular_speed <dbl>,
#> #   mad_angular_speed <dbl>, median_angular_velocity <dbl>,
#> #   mad_angular_velocity <dbl>, median_angular_acceleration <dbl>,
#> #   mad_angular_acceleration <dbl>, median_heading <dbl>, mad_heading <dbl>,
#> #   total_path_length <dbl>, total_angular_path_length <dbl>,
#> #   net_displacement <dbl>, straightness <dbl>, sinuosity <dbl>, emax <dbl>

# Tortuosity measures instead of kinematics
summarise_aniframe(calculate_tortuosity(kin), type = "tortuosity")
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_path_length total_angular_path_length
#>        <int> <fct>      <int> <int>             <dbl>                     <dbl>
#> 1          1 centroid       1     1              39.7                      25.3
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, emax <dbl>
```

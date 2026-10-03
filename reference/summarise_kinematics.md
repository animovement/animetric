# Calculate kinematic summary statistics

Calculate central tendency and dispersion for translational and
rotational kinematics.

## Usage

``` r
summarise_kinematics(
  data,
  measures = c("median_mad", "mean_sd"),
  .check = TRUE
)

summarize_kinematics(
  data,
  measures = c("median_mad", "mean_sd"),
  .check = TRUE
)
```

## Arguments

- data:

  A kinematics anipoint (output of
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md))

- measures:

  Measures of central tendency and dispersion for kinematics. Options
  are `"median_mad"` (default) and `"mean_sd"`.

- .check:

  Whether to validate input. Set to `FALSE` when called from
  [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  to avoid redundant checks.

## Value

A summarised data frame with one row per group containing central
tendency and dispersion measures (prefixed with median\_/mad\_ or
mean\_/sd\_)

- Speed, acceleration

- Turning speed (2D and 3D)

- Turning rate and acceleration, and course (circular statistics): 2D,
  and 3D when
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  was given a `vertical`

- Course elevation (3D with a `vertical`)

Angular summaries are in the frame's declared `unit_angle`.

## Examples

``` r
kin <- calculate_kinematics(
  anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
summarise_kinematics(kin)
#> # A tibble: 1 × 16
#>   individual keypoint session trial median_speed mad_speed median_acceleration
#>        <int> <fct>      <int> <int>        <dbl>     <dbl>               <dbl>
#> 1          1 centroid       1     1        0.837     0.581             -0.0164
#> # ℹ 9 more variables: mad_acceleration <dbl>, median_turning_speed <dbl>,
#> #   mad_turning_speed <dbl>, median_turning_rate <dbl>, mad_turning_rate <dbl>,
#> #   median_turning_acceleration <dbl>, mad_turning_acceleration <dbl>,
#> #   median_course <dbl>, mad_course <dbl>

# Mean and standard deviation instead of median and MAD
summarise_kinematics(kin, measures = "mean_sd")
#> # A tibble: 1 × 16
#>   individual keypoint session trial mean_speed sd_speed mean_acceleration
#>        <int> <fct>      <int> <int>      <dbl>    <dbl>             <dbl>
#> 1          1 centroid       1     1      0.938    0.469            0.0128
#> # ℹ 9 more variables: sd_acceleration <dbl>, mean_turning_speed <dbl>,
#> #   sd_turning_speed <dbl>, mean_turning_rate <dbl>, sd_turning_rate <dbl>,
#> #   mean_turning_acceleration <dbl>, sd_turning_acceleration <dbl>,
#> #   mean_course <dbl>, sd_course <dbl>
```

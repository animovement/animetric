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

  A kinematics aniframe (output of
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

- Angular speed, velocity, acceleration (2D only)

- Heading (2D only, using circular statistics)

## Examples

``` r
kin <- calculate_kinematics(
  anicore::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
summarise_kinematics(kin)
#> # A tibble: 1 × 16
#>   individual keypoint session trial median_speed mad_speed median_acceleration
#>        <int> <fct>      <int> <int>        <dbl>     <dbl>               <dbl>
#> 1          1 centroid       1     1        0.912     0.423             -0.0100
#> # ℹ 9 more variables: mad_acceleration <dbl>, median_angular_speed <dbl>,
#> #   mad_angular_speed <dbl>, median_angular_velocity <dbl>,
#> #   mad_angular_velocity <dbl>, median_angular_acceleration <dbl>,
#> #   mad_angular_acceleration <dbl>, median_heading <dbl>, mad_heading <dbl>

# Mean and standard deviation instead of median and MAD
summarise_kinematics(kin, measures = "mean_sd")
#> # A tibble: 1 × 16
#>   individual keypoint session trial mean_speed sd_speed mean_acceleration
#>        <int> <fct>      <int> <int>      <dbl>    <dbl>             <dbl>
#> 1          1 centroid       1     1      0.886    0.395           0.00981
#> # ℹ 9 more variables: sd_acceleration <dbl>, mean_angular_speed <dbl>,
#> #   sd_angular_speed <dbl>, mean_angular_velocity <dbl>,
#> #   sd_angular_velocity <dbl>, mean_angular_acceleration <dbl>,
#> #   sd_angular_acceleration <dbl>, mean_heading <dbl>, sd_heading <dbl>
```

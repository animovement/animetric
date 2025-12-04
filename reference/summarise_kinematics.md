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
  [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md))

- measures:

  Measures of central tendency and dispersion for kinematics. Options
  are `"median_mad"` (default) and `"mean_sd"`.

- .check:

  Whether to validate input. Set to `FALSE` when called from
  [`summarise_aniframe()`](http://animovement.dev/animetric/reference/summarise_aniframe.md)
  to avoid redundant checks.

## Value

A summarised data frame with one row per group containing central
tendency and dispersion measures (prefixed with median\_/mad\_ or
mean\_/sd\_)

- Speed, acceleration

- Angular speed, velocity, acceleration (2D only)

- Heading (2D only, using circular statistics)

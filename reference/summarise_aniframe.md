# Summarise the time series of an aniframe

**\[experimental\]**

Summarises the per-row measures of a frame over each group: a measure of
central tendency and of dispersion for each, one row per group. It
describes the measures the frame already carries — the output of
[`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
[`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
or your own `mutate()` — and computes no new ones. Any grouping is
allowed, since a median of rows means the same whether the rows are one
keypoint's or a whole animal's.

Properties of a trajectory as a whole, such as how far it ended up from
where it started, are not a statistic of any column; see
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md).

Each frame class has its own default set of measures:

- anipoint:

  `speed`, `acceleration`, `turning_speed`, `turning_rate`,
  `turning_acceleration`, `course_elevation`, the windowed
  `straightness`, `sinuosity` and `emax`, and `confidence`; circular
  statistics for `course` and for a declared `yaw`, reported as
  `*_heading`.

- anisegment:

  `length` and `confidence`.

- anijoint:

  `angle`, with circular statistics, and `confidence`.

Only the measures present are summarised. Velocity and acceleration
components, `course_unwrapped`, and the running totals `path_length` and
`cumulative_turning` are left out by default;
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
reports the totals.

Angles are summarised with circular statistics
([`anicore::circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.html)
and its siblings), so that the mean of 350 and 10 degrees is 0 rather
than 180. They are computed in radians and reported in the frame's
`unit_angle`. Only the angles animetric knows of are treated as
circular; any other column is summarised as a linear quantity.

## Usage

``` r
summarise_aniframe(data, ...)

summarize_aniframe(data, ...)

# S3 method for class 'anipoint'
summarise_aniframe(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
)

# S3 method for class 'anisegment'
summarise_aniframe(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
)

# S3 method for class 'anijoint'
summarise_aniframe(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
)

# S3 method for class 'anievent'
summarise_aniframe(data, ...)

# Default S3 method
summarise_aniframe(data, ...)
```

## Arguments

- data:

  An anipoint, anisegment or anijoint.

- ...:

  Not used. Passing `type` (the `"kinematics"` and `"tortuosity"`
  summaries this function used to combine) is deprecated: use
  `summarise_aniframe()` and
  [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
  instead.

- cols:

  Character vector of columns to summarise, in place of the class's
  default set. A known angle among them is still summarised with
  circular statistics.

- measures:

  Measures of central tendency and dispersion: `"median_mad"` (default)
  or `"mean_sd"`.

## Value

A data frame with one row per group: the grouping columns, then
`<measure>_<column>` for each measure, e.g. `median_speed` and
`mad_speed`.

## See also

[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
for whole-trajectory measures.

## Examples

``` r
kin <- calculate_kinematics(
  anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
summarise_aniframe(kin)
#> # A tibble: 1 × 18
#>   individual keypoint session trial median_speed mad_speed median_acceleration
#>        <int> <fct>      <int> <int>        <dbl>     <dbl>               <dbl>
#> 1          1 centroid       1     1        0.743     0.480              0.0554
#> # ℹ 11 more variables: mad_acceleration <dbl>, median_turning_speed <dbl>,
#> #   mad_turning_speed <dbl>, median_turning_rate <dbl>, mad_turning_rate <dbl>,
#> #   median_turning_acceleration <dbl>, mad_turning_acceleration <dbl>,
#> #   median_confidence <dbl>, mad_confidence <dbl>, median_course <dbl>,
#> #   mad_course <dbl>

# Mean and standard deviation instead of median and MAD
summarise_aniframe(kin, measures = "mean_sd")
#> # A tibble: 1 × 18
#>   individual keypoint session trial mean_speed sd_speed mean_acceleration
#>        <int> <fct>      <int> <int>      <dbl>    <dbl>             <dbl>
#> 1          1 centroid       1     1      0.846    0.522            0.0264
#> # ℹ 11 more variables: sd_acceleration <dbl>, mean_turning_speed <dbl>,
#> #   sd_turning_speed <dbl>, mean_turning_rate <dbl>, sd_turning_rate <dbl>,
#> #   mean_turning_acceleration <dbl>, sd_turning_acceleration <dbl>,
#> #   mean_confidence <dbl>, sd_confidence <dbl>, mean_course <dbl>,
#> #   sd_course <dbl>

# Only some measures
summarise_aniframe(kin, cols = c("speed", "course"))
#> # A tibble: 1 × 8
#>   individual keypoint session trial median_speed mad_speed median_course
#>        <int> <fct>      <int> <int>        <dbl>     <dbl>         <dbl>
#> 1          1 centroid       1     1        0.743     0.480          1.84
#> # ℹ 1 more variable: mad_course <dbl>
```

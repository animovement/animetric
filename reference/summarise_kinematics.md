# Calculate kinematic summary statistics

**Deprecated.** Use
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md),
which summarises any per-row measure of a frame, not only kinematics.
This keeps the old output: the median and MAD (or mean and SD) of
`speed`, `acceleration`, the turning measures, `course_elevation` and,
with circular statistics, `course`.

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

  An anipoint with kinematic columns.

- measures:

  `"median_mad"` (default) or `"mean_sd"`.

- .check:

  Ignored.

## Value

A data frame with one row per group.

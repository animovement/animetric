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
  [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md))

- type:

  Character vector of summary types. Options are `"kinematics"` and
  `"tortuosity"`. Default is both.

- measures:

  Measures of central tendency and dispersion for kinematics. Options
  are `"median_mad"` (default) and `"mean_sd"`.

## Value

A summarised data frame with one row per group.

## See also

[`summarise_kinematics()`](http://animovement.dev/animetric/reference/summarise_kinematics.md),
[`summarise_tortuosity()`](http://animovement.dev/animetric/reference/summarise_tortuosity.md)

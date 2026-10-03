# Summaries deriving a declared orientation

Summaries deriving a declared orientation

## Usage

``` r
orientation_summaries(orientation_cols, unit, signed_yaw, weighted)
```

## Arguments

- orientation_cols:

  Named character vector, orientation role to column, as from
  `anicore::get_variables(data, "where", "orientation")`.

- unit:

  The frame's `unit_angle`.

- signed_yaw:

  Whether yaw is kept in `(-pi, pi]` rather than `[0, 2pi)`, following
  the input.

- weighted:

  Whether to weight by `confidence`.

## Value

A named list of quosures, for
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).

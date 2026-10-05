# The step length to rediscretise a trajectory at, as asked for

The step length to rediscretise a trajectory at, as asked for

## Usage

``` r
resolve_step_length(step_length, position)
```

## Arguments

- step_length:

  See
  [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
  already checked.

- position:

  A data frame of positions, one column per axis.

## Value

A number: `step_length` itself, or for `"auto"` the trajectory's
[`mean_step_length()`](https://animovement.dev/animetric/reference/mean_step_length.md).

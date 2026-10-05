# Sinuosity and E_max of a rediscretised path, over sliding windows

The path is rediscretised once, at the step from
[`resolve_step_length()`](https://animovement.dev/animetric/reference/resolve_step_length.md):
by default the trajectory's step length from
[`mean_step_length()`](https://animovement.dev/animetric/reference/mean_step_length.md).
Each row's window spans the same rows as its straightness, and takes the
turning at the rediscretised points the path reaches within it.

## Usage

``` r
window_sinuosity(position, time, window_width, step_length = "auto")
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- time:

  The index.

- window_width:

  The window width, in rows.

- step_length:

  The step to rediscretise at, as for
  [`resolve_step_length()`](https://animovement.dev/animetric/reference/resolve_step_length.md).

## Value

A list of two numeric vectors, `sinuosity` and `e_max`. `NA` where the
window runs past either end, or holds no turning.

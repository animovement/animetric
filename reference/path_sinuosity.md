# Sinuosity and E_max of a rediscretised path

Sinuosity and E_max of a rediscretised path

## Usage

``` r
path_sinuosity(position, time, step_length = "auto")
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- time:

  The index.

- step_length:

  The step to rediscretise at, as for
  [`resolve_step_length()`](https://animovement.dev/animetric/reference/resolve_step_length.md).

## Value

A list of `sinuosity` and `e_max`, each a number.

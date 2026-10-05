# The measures of each whole trajectory

The measures of each whole trajectory

## Usage

``` r
path_summary(
  data,
  min_step,
  rediscretise,
  step_length = "auto",
  call = rlang::caller_env()
)
```

## Arguments

- data:

  An anipoint.

- min_step:

  See
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md).

- rediscretise:

  Whether sinuosity and E_max come from the path rediscretised to a
  constant step length, or, as
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  computed them, from the turning between successive frames.

- step_length:

  The step to rediscretise at, see
  [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md).
  Ignored when `rediscretise` is `FALSE`.

- call:

  The calling environment, for error messages.

## Value

A data frame with one row per trajectory.

# The minimum step of one trajectory

The minimum step of one trajectory

## Usage

``` r
resolve_min_step(min_step, position, velocity, time)
```

## Arguments

- min_step:

  See
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md),
  already checked.

- position:

  A data frame of positions, one column per axis.

- velocity:

  A data frame of velocities, one column per axis.

- time:

  The index.

## Value

A number: `min_step` itself, or for `"auto"` the threshold from
[`auto_min_step()`](https://animovement.dev/animetric/reference/auto_min_step.md).

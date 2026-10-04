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

A number: `min_step` itself, or for `"auto"` three times the noise from
[`positional_noise()`](https://animovement.dev/animetric/reference/positional_noise.md),
at most half the median step.

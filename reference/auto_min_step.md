# The minimum step `"auto"` chooses for one trajectory

The one place the automatic threshold is computed, for both
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
and
[`compute_min_step()`](https://animovement.dev/animetric/reference/compute_min_step.md).

## Usage

``` r
auto_min_step(position, velocity, time)
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- velocity:

  A data frame of velocities, one column per axis.

- time:

  The index.

## Value

A one-row data frame: `positional_noise`, from
[`positional_noise()`](https://animovement.dev/animetric/reference/positional_noise.md),
and `min_step`, three times that noise, at most half the median step.

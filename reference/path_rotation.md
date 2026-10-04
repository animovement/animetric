# The turning measures of one trajectory

Every rate is the derivative of an angle, not a formula in velocity and
acceleration: `|v x a| / |v|^2` measures the sine of a turn, which falls
back toward 0 as the turn nears pi, so a sharp reversal in jittery
tracking would read as no turn at all.

## Usage

``` r
path_rotation(velocity, time, up, min_step = 0)
```

## Arguments

- velocity:

  A data frame of velocity components, one column per axis.

- time:

  The index.

- up:

  The unit vertical, as from
  [`vertical_vector()`](https://animovement.dev/animetric/reference/vertical_vector.md),
  or `NULL` for only the measures that need none.

- min_step:

  The shortest step whose direction counts, a number.

## Value

A data frame of turning measures, in radians.

# Positional noise of a trajectory

The robust standard deviation of the second differences of position
along the direction of travel, over `sqrt(6)`: for white noise of
standard deviation `sigma` on each axis, a second difference has
standard deviation `sqrt(6) * sigma` in any direction. Along the
direction of travel, a path turning at constant speed contributes
nothing.

## Usage

``` r
positional_noise(position, velocity)
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- velocity:

  A data frame of velocities, one column per axis.

## Value

A number, in the unit of the positions. `0` when there are too few rows,
or too little movement, to estimate it.

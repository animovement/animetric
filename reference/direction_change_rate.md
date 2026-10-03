# How fast the direction of travel changes, in any number of dimensions

The angle between the velocities either side of each row, over the time
between them: a central difference of the direction, with one-sided ones
at the ends. It needs no reference direction, so it has no wrap at +/-pi
and no singularity when travel is vertical.

## Usage

``` r
direction_change_rate(v, time)
```

## Arguments

- v:

  Numeric matrix of velocities, one row per observation.

- time:

  The index.

## Value

Numeric vector, in radians per unit of `time`. `NA` where either
neighbouring velocity is zero or missing.

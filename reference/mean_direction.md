# The mean direction of a set of angles

The mean direction of a set of angles

## Usage

``` r
mean_direction(x, w = NULL, signed = TRUE)
```

## Arguments

- x:

  Numeric vector of angles, in radians.

- w:

  Weights, or `NULL` for equal ones.

- signed:

  Whether to return the direction in `(-pi, pi]` rather than `[0, 2pi)`.

## Value

One angle, in radians; `NA` when no angle is present or they cancel out.

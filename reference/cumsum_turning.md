# Cumulative absolute turning of an angle series

Starts at 0 and accumulates the absolute turn between consecutive
non-`NA` angles, taken the shorter way round the circle, so turning
across a gap is counted once, where the angle is next defined. Rows with
an `NA` angle carry the running total.

## Usage

``` r
cumsum_turning(x)
```

## Arguments

- x:

  Numeric vector of angles, in radians. They need not be unwrapped.

## Value

Numeric vector of the same length as `x`.

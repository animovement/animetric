# Compute numerical derivatives on possibly uneven grids

This helper implements a finite-difference scheme based on the
**Fornberg formula** for interior points when the spacing between
coordinates is non-uniform. End points use first-order forward/backward
differences.

## Usage

``` r
compute_gradient(values, coords = NULL)
```

## Arguments

- values:

  Numeric vector of function values.

- coords:

  Numeric vector of the corresponding coordinate values. Must be the
  same length as `values`.

## Value

A numeric vector of the same length as `values` containing the estimated
first derivative at each point.

## References

Fornberg, B. (1988). Generation of finite difference formulas on
arbitrarily spaced grids. *Mathematics of Computation*, 51(184),
699-706.
[doi:10.1090/S0025-5718-1988-0935077-0](https://doi.org/10.1090/S0025-5718-1988-0935077-0)

## Examples

``` r
# The derivative of x^2 is 2x, which the interior points recover exactly
compute_gradient(c(0, 1, 4, 9, 16))
#> [1] 1 2 4 6 7

# The spacing between coordinates need not be even
compute_gradient(c(0, 1, 4, 9), coords = c(0, 1, 3, 6))
#> [1] 1.000000 1.166667 1.566667 1.666667
```

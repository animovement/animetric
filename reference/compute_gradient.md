# Compute numerical derivatives on possibly uneven grids

This helper implements a finite‑difference scheme based on the
**Fornberg formula** for interior points when the spacing between
coordinates is non‑uniform. End points use first‑order forward/backward
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

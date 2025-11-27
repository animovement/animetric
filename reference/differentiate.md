# Differentiate a numeric series (optionally repeatedly)

Wrapper around
[`compute_gradient()`](http://animovement.dev/animetric/reference/compute_gradient.md)
that optionally applies the gradient operator multiple times (`order`).
If no explicit time vector is supplied, a simple index sequence is used.

## Usage

``` r
differentiate(x, time = NULL, order = 1)
```

## Arguments

- x:

  Numeric vector of observations.

- time:

  Optional numeric vector of timestamps. Must be the same length as `x`.
  If `NULL`, `seq_along(x)` is used.

- order:

  Integer ≥ 1 indicating how many times the differentiation should be
  applied. Defaults to a single derivative.

## Value

Numeric vector of the same length as `x` containing the differentiated
values.

## Details

The function computes the first‑order derivative using the
Fornberg‑based scheme implemented in
[`compute_gradient()`](http://animovement.dev/animetric/reference/compute_gradient.md).
When `order > 1`, the gradient is applied iteratively to the result of
the previous iteration.

## Examples

``` r
  # Simple equally spaced case
  y <- sin(seq(0, 2 * pi, length.out = 10))
  differentiate(y)
#>  [1]  0.6427876  0.4924039  0.1116189 -0.3213938 -0.6040228 -0.6040228
#>  [7] -0.3213938  0.1116189  0.4924039  0.6427876

  # Uneven time stamps
  t <- c(0, 0.9, 2.1, 3.8, 5.0, 5.2, 5.6, 6.7, 7.2, 8.9)
  differentiate(y, time = t, order = 2)
#>  [1] -0.2043770 -0.2568236 -0.2945202 -1.4263820  0.8653926  2.3633693
#>  [7]  3.5124758  0.6498348  0.2434526 -0.1390591
```

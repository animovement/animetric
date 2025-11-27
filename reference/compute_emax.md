# Compute E_max (maximum expected displacement) from pre‑computed vectors

Compute E_max (maximum expected displacement) from pre‑computed vectors

## Usage

``` r
compute_emax(mean_cos_turning, mean_step_length = NULL, dimensional = FALSE)
```

## Arguments

- mean_cos_turning:

  Numeric vector of mean cosine of turning angles.

- mean_step_length:

  Numeric vector of mean step lengths (required if
  `dimensional = TRUE`).

- dimensional:

  Logical. If `TRUE`, returns E_max in spatial units; otherwise returns
  the dimensionless ratio.

## Value

Numeric vector of E_max values (same length as `mean_cos_turning`), with
`NA` for invalid inputs and `Inf` for perfectly straight paths.

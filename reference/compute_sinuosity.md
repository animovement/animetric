# Compute sinuosity index from precomputed vectors

Compute sinuosity index from precomputed vectors

## Usage

``` r
compute_sinuosity(
  mean_step_length,
  mean_cos_turning,
  method = c("corrected", "original")
)
```

## Arguments

- mean_step_length:

  Numeric vector of mean step lengths within window

- mean_cos_turning:

  Numeric vector of mean cosine of turning angles

- method:

  Either "corrected" (Benhamou 2004) or "original" (Bovet & Benhamou
  1988)

## Value

Numeric vector of sinuosity values

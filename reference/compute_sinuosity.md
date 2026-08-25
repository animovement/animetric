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

## Examples

``` r
compute_sinuosity(mean_step_length = 1.2, mean_cos_turning = 0.8)
#> [1] 0.6085806

# The original formulation, for comparison with older work
compute_sinuosity(1.2, 0.8, method = "original")
#> [1] 4.082483
```

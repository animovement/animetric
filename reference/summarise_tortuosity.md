# Calculate tortuosity summary statistics

Calculate path length, displacement, and tortuosity metrics.

## Usage

``` r
summarise_tortuosity(data)

summarize_tortuosity(data)
```

## Arguments

- data:

  A kinematics aniframe (output of
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md))

## Value

A summarised data frame with one row per group containing:

- `total_path_length`: Total distance traveled

- `total_angular_path_length`: Total angular distance (2D only)

**Tortuosity metrics:**

- `net_displacement`: Straight-line distance from start to end

- `straightness`: Ratio of net displacement to path length (0-1)

- `sinuosity`: Corrected sinuosity index (Benhamou 2004)

- `emax`: Maximum expected displacement (dimensionless)

## References

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal's path. Journal of Theoretical Biology, 229(2), 209-220.

## Examples

``` r
kin <- calculate_kinematics(
  aniframe::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
summarise_tortuosity(calculate_tortuosity(kin))
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_path_length total_angular_path_length
#>        <int> <fct>      <int> <int>             <dbl>                     <dbl>
#> 1          1 centroid       1     1              24.0                      25.9
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, emax <dbl>
```

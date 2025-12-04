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
  [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md))

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

# Calculate summary statistics

Calculate summary statistics for tracks, including translational
kinematics, rotational kinematics, and tortuosity metrics.

## Usage

``` r
summarise_kinematics(data, measures = c("median_mad", "mean_sd"))
```

## Arguments

- data:

  A kinematics aniframe (output of
  [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md))

- measures:

  Measures of central tendency and dispersion. Options are
  `"median_mad"` (default) and `"mean_sd"`.

## Value

A summarised data frame with one row per group containing:

**Totals:**

- `total_path_length`: Total distance traveled

**Tortuosity metrics:**

- `straightness`: Ratio of net displacement to path length (0-1)

- `sinuosity`: Corrected sinuosity index (Benhamou 2004)

- `emax`: Maximum expected displacement (dimensionless)

**Central tendency and dispersion** (prefixed with median\_/mad\_ or
mean\_/sd\_):

- Speed, acceleration

- Angular speed, velocity, acceleration (2D only)

- Heading (2D only, using circular statistics)

## References

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal's path. Journal of Theoretical Biology, 229(2), 209-220.

# Calculate tortuosity metrics over sliding windows

Computes multiple tortuosity metrics (straightness, sinuosity, E_max)
over sliding windows, returning a value at each timepoint.

## Usage

``` r
calculate_tortuosity(data, window_width = 11L)
```

## Arguments

- data:

  An aniframe with position coordinates and time. Velocity and heading
  columns will be computed if not already present.

- window_width:

  Size of the sliding window (number of observations). Should be an odd
  number \>= 3 for symmetric centering.

## Value

The input aniframe with additional columns:

- straightness:

  Straightness index (D/L), ranges 0-1

- sinuosity:

  Corrected sinuosity index (Benhamou 2004)

- emax:

  Maximum expected displacement (dimensionless)

## Details

If required kinematic columns are missing, the function will compute
them automatically by calling the appropriate helper functions.

Straightness is appropriate for directed/goal-oriented movement, while
sinuosity and E_max are appropriate for random search paths.

For 2D data, heading is derived from the velocity vector, which provides
smoother estimates than raw position differences.

For 3D data, turning angles are computed as the angle between
consecutive velocity vectors using the dot product.

The window is centered on each timepoint. At path edges, metrics are
computed from available data within the truncated window.

## References

Batschelet, E. (1981). Circular statistics in biology. Academic Press.

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal’s path: straightness, sinuosity, or fractal dimension?. Journal
of Theoretical Biology, 229(2), 209-220.

Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2007). Animal
navigation: the difficulty of moving in a straight line. Biological
Cybernetics, 97(1), 47-61.

## See also

- [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md)
  for computing velocity and heading

## Examples

``` r
if (FALSE) { # \dontrun{
# Kinematics computed automatically if missing
data |>
  calculate_tortuosity(window_width = 11)

# Or with kinematics already computed
data |>
  calculate_kinematics() |>
  calculate_tortuosity(window_width = 11)
} # }
```

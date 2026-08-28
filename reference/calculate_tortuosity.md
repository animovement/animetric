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

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  for computing velocity and heading

## Examples

``` r
data <- anicore::example_aniframe(n_obs = 30, n_individuals = 1, n_keypoints = 1)

# Kinematics computed automatically if missing
data |>
  calculate_tortuosity(window_width = 11)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence speed
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl> <dbl>
#>  1          1 centroid       1     1     1  0.614   0.749      0.836 2.94 
#>  2          1 centroid       1     1     2 -0.618  -1.92       0.835 0.323
#>  3          1 centroid       1     1     3  0.221   0.236      0.593 1.54 
#>  4          1 centroid       1     1     4  1.13    0.629      0.774 0.802
#>  5          1 centroid       1     1     5  1.81    0.418      0.855 0.906
#>  6          1 centroid       1     1     6 -0.0838  1.98       0.914 0.513
#>  7          1 centroid       1     1     7  1.37   -0.506      0.852 1.57 
#>  8          1 centroid       1     1     8 -0.627  -1.11       0.826 0.822
#>  9          1 centroid       1     1     9 -0.217  -0.949      0.521 0.794
#> 10          1 centroid       1     1    10 -0.684   0.477      0.649 0.137
#> # ℹ 20 more rows
#> # ℹ 15 more variables: acceleration <dbl>, path_length <dbl>, v_x <dbl>,
#> #   v_y <dbl>, a_x <dbl>, a_y <dbl>, heading <dbl>, heading_unwrapped <dbl>,
#> #   angular_speed <dbl>, angular_velocity <dbl>, angular_acceleration <dbl>,
#> #   angular_path_length <dbl>, straightness <dbl>, sinuosity <dbl>, emax <dbl>

# Or with kinematics already computed
data |>
  calculate_kinematics() |>
  calculate_tortuosity(window_width = 11)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence speed
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl> <dbl>
#>  1          1 centroid       1     1     1  0.614   0.749      0.836 2.94 
#>  2          1 centroid       1     1     2 -0.618  -1.92       0.835 0.323
#>  3          1 centroid       1     1     3  0.221   0.236      0.593 1.54 
#>  4          1 centroid       1     1     4  1.13    0.629      0.774 0.802
#>  5          1 centroid       1     1     5  1.81    0.418      0.855 0.906
#>  6          1 centroid       1     1     6 -0.0838  1.98       0.914 0.513
#>  7          1 centroid       1     1     7  1.37   -0.506      0.852 1.57 
#>  8          1 centroid       1     1     8 -0.627  -1.11       0.826 0.822
#>  9          1 centroid       1     1     9 -0.217  -0.949      0.521 0.794
#> 10          1 centroid       1     1    10 -0.684   0.477      0.649 0.137
#> # ℹ 20 more rows
#> # ℹ 15 more variables: acceleration <dbl>, path_length <dbl>, v_x <dbl>,
#> #   v_y <dbl>, a_x <dbl>, a_y <dbl>, heading <dbl>, heading_unwrapped <dbl>,
#> #   angular_speed <dbl>, angular_velocity <dbl>, angular_acceleration <dbl>,
#> #   angular_path_length <dbl>, straightness <dbl>, sinuosity <dbl>, emax <dbl>
```

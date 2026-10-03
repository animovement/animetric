# Calculate tortuosity metrics over sliding windows

Computes multiple tortuosity metrics (straightness, sinuosity, E_max)
over sliding windows, returning a value at each timepoint.

## Usage

``` r
calculate_tortuosity(data, window_width = 11L)
```

## Arguments

- data:

  A Cartesian anipoint. Kinematic columns will be computed if not
  already present.

- window_width:

  Size of the sliding window (number of observations). Should be an odd
  number \>= 3 for symmetric centering.

## Value

The input anipoint with additional columns:

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

Works on 1D, 2D and 3D Cartesian data, reading the axes from the frame's
declared variables. Turning angles are the angles between consecutive
velocity vectors
([`anicore::angle_between()`](https://animovement.dev/anicore/reference/angle_between.html)),
which gives smoother estimates than raw position differences.

The window is centered on each timepoint. Near the ends of a trajectory,
where the window would run past the first or last observation, the
metrics are `NA`.

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
  for computing velocity and course

## Examples

``` r
data <- anicore::example_anipoint(n_obs = 30, n_individuals = 1, n_keypoints = 1)

# Kinematics computed automatically if missing
data |>
  calculate_tortuosity(window_width = 11)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence speed
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl> <dbl>
#>  1          1 centroid       1     1     1  0.889   1.18       0.724 1.14 
#>  2          1 centroid       1     1     2  0.0132  0.447      0.623 0.641
#>  3          1 centroid       1     1     3  0.225   2.27       0.825 0.403
#>  4          1 centroid       1     1     4 -0.730   0.136      0.759 2.26 
#>  5          1 centroid       1     1     5 -1.22   -2.00       0.939 0.633
#>  6          1 centroid       1     1     6  0.407  -0.421      0.928 0.844
#>  7          1 centroid       1     1     7 -0.751  -0.378      0.965 0.869
#>  8          1 centroid       1     1     8 -0.162   1.22       0.843 0.801
#>  9          1 centroid       1     1     9  0.352  -1.54       0.741 0.768
#> 10          1 centroid       1     1    10 -0.289  -0.310      0.469 0.770
#> # ℹ 20 more rows
#> # ℹ 15 more variables: acceleration <dbl>, path_length <dbl>, v_x <dbl>,
#> #   v_y <dbl>, a_x <dbl>, a_y <dbl>, course <dbl>, course_unwrapped <dbl>,
#> #   turning_speed <dbl>, turning_rate <dbl>, turning_acceleration <dbl>,
#> #   cumulative_turning <dbl>, straightness <dbl>, sinuosity <dbl>, emax <dbl>

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
#>  1          1 centroid       1     1     1  0.889   1.18       0.724 1.14 
#>  2          1 centroid       1     1     2  0.0132  0.447      0.623 0.641
#>  3          1 centroid       1     1     3  0.225   2.27       0.825 0.403
#>  4          1 centroid       1     1     4 -0.730   0.136      0.759 2.26 
#>  5          1 centroid       1     1     5 -1.22   -2.00       0.939 0.633
#>  6          1 centroid       1     1     6  0.407  -0.421      0.928 0.844
#>  7          1 centroid       1     1     7 -0.751  -0.378      0.965 0.869
#>  8          1 centroid       1     1     8 -0.162   1.22       0.843 0.801
#>  9          1 centroid       1     1     9  0.352  -1.54       0.741 0.768
#> 10          1 centroid       1     1    10 -0.289  -0.310      0.469 0.770
#> # ℹ 20 more rows
#> # ℹ 15 more variables: acceleration <dbl>, path_length <dbl>, v_x <dbl>,
#> #   v_y <dbl>, a_x <dbl>, a_y <dbl>, course <dbl>, course_unwrapped <dbl>,
#> #   turning_speed <dbl>, turning_rate <dbl>, turning_acceleration <dbl>,
#> #   cumulative_turning <dbl>, straightness <dbl>, sinuosity <dbl>, emax <dbl>
```

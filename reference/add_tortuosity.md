# Add tortuosity measures over sliding windows

Computes how winding the path is (straightness, sinuosity and E_max)
over a window centred on each row, and returns the frame with a column
for each. Everything they need is computed internally and not added, so
only the three measures appear.

## Usage

``` r
add_tortuosity(data, window_width = 11L)
```

## Arguments

- data:

  A Cartesian anipoint.

- window_width:

  Size of the sliding window, in observations (default `11L`). Should be
  an odd number \>= 3 for symmetric centering.

## Value

The input anipoint with three columns added, named with the window
width, so that `window_width = 11` gives:

- `straightness_11`:

  Straightness index (D/L), from 0 to 1.

- `sinuosity_11`:

  Corrected sinuosity index (Benhamou 2004).

- `e_max_11`:

  Maximum expected displacement (dimensionless).

The width in the name keeps these windowed measures apart from the
whole-path `straightness`, `sinuosity` and `e_max` of
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
and lets several window widths sit side by side.

## Details

Straightness is appropriate for directed/goal-oriented movement, while
sinuosity and E_max are appropriate for random search paths.

Works on 1D, 2D and 3D Cartesian data, reading the axes from the frame's
declared variables.

The window is centered on each timepoint. Near the ends of a trajectory,
where the window would run past the first or last observation, the
metrics are `NA`.

**Straightness** is the distance between the positions at the ends of
the window over the distance travelled between them.

**Sinuosity and E_max** come from the turning angles of the path
rediscretised to a constant step length, as Benhamou (2004) defines
sinuosity: walking along the path, a new point is placed wherever it
first leaves a circle of that radius around the last one (Bovet &
Benhamou 1988). The step length is the trajectory's mean step between
rows, weighted by step length: the average step over the distance
travelled, which time spent still does not shorten. Tracking jitter that
stays within the circle while an animal is still gives no steps and no
turning, where turning angles between successive frames would be
dominated by it. Each window takes the turning at the rediscretised
points the path reaches within it, and is `NA` when there are none:
where the animal moved less than a step, its path has no sinuosity to
measure. The path is rediscretised once per trajectory, and missing
positions break it into stretches that are rediscretised separately.

## References

Batschelet, E. (1981). Circular statistics in biology. Academic Press.

Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals' movements
using a correlated random walk model. Journal of Theoretical Biology,
131(4), 419-433.

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal’s path: straightness, sinuosity, or fractal dimension?. Journal
of Theoretical Biology, 229(2), 209-220.

Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2007). Animal
navigation: the difficulty of moving in a straight line. Biological
Cybernetics, 97(1), 47-61.

## See also

- [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
  for speed, course and the turning measures.

- [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
  for the same measures over each whole trajectory.

## Examples

``` r
data <- anicore::example_anipoint(n_obs = 30, n_individuals = 1, n_keypoints = 1)

data |>
  add_tortuosity(window_width = 11)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1  0.198  -0.840       0.657
#>  2          1 centroid       1     1     2 -1.20   -1.35        0.610
#>  3          1 centroid       1     1     3 -0.0398 -0.818       0.546
#>  4          1 centroid       1     1     4  0.687  -0.634       0.574
#>  5          1 centroid       1     1     5  0.705   0.816       0.823
#>  6          1 centroid       1     1     6  0.991   0.303       0.668
#>  7          1 centroid       1     1     7  1.14    1.81        0.434
#>  8          1 centroid       1     1     8 -1.24   -0.894       0.731
#>  9          1 centroid       1     1     9  2.65   -0.0464      0.365
#> 10          1 centroid       1     1    10 -0.157  -0.471       0.825
#> # ℹ 20 more rows
#> # ℹ 3 more variables: straightness_11 <dbl>, sinuosity_11 <dbl>, e_max_11 <dbl>

# Several window widths side by side
data |>
  add_tortuosity(window_width = 5) |>
  add_tortuosity(window_width = 11)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 centroid       1     1     1  0.198  -0.840       0.657
#>  2          1 centroid       1     1     2 -1.20   -1.35        0.610
#>  3          1 centroid       1     1     3 -0.0398 -0.818       0.546
#>  4          1 centroid       1     1     4  0.687  -0.634       0.574
#>  5          1 centroid       1     1     5  0.705   0.816       0.823
#>  6          1 centroid       1     1     6  0.991   0.303       0.668
#>  7          1 centroid       1     1     7  1.14    1.81        0.434
#>  8          1 centroid       1     1     8 -1.24   -0.894       0.731
#>  9          1 centroid       1     1     9  2.65   -0.0464      0.365
#> 10          1 centroid       1     1    10 -0.157  -0.471       0.825
#> # ℹ 20 more rows
#> # ℹ 6 more variables: straightness_5 <dbl>, sinuosity_5 <dbl>, e_max_5 <dbl>,
#> #   straightness_11 <dbl>, sinuosity_11 <dbl>, e_max_11 <dbl>
```

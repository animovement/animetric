# Add tortuosity measures over sliding windows

Computes how winding the path is (straightness, sinuosity and E_max)
over a window centred on each row, and returns the frame with a column
for each. Everything they need is computed internally and not added, so
only the three measures appear.

## Usage

``` r
add_tortuosity(data, window_width = 11L, step_length = "auto")
```

## Arguments

- data:

  A Cartesian anipoint.

- window_width:

  Size of the sliding window, in observations (default `11L`). Should be
  an odd number \>= 3 for symmetric centering.

- step_length:

  **\[experimental\]** The step length the path is rediscretised at for
  sinuosity and E_max, in the frame's spatial unit (default `"auto"`).
  `"auto"` takes each trajectory's mean step, weighted by step length
  (see Details). A positive number rediscretises every trajectory at
  that step, so that their sinuosity can be compared at one scale.
  Straightness does not use it.

## Value

The input anipoint with three columns added, named with the window
width, so that `window_width = 11` gives:

- `straightness_11`:

  Straightness index (D/L), from 0 to 1. `NA` where a position in the
  window is missing.

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
the window over the distance travelled between them. It is `NA` for a
window that holds a missing position: how far the animal moved across
the gap is not known, so neither is the distance travelled.

**Sinuosity and E_max** come from the turning angles of the path
rediscretised to a constant step length, as Benhamou (2004) defines
sinuosity: walking along the path, a new point is placed wherever it
first leaves a circle of that radius around the last one (Bovet &
Benhamou 1988). By default (`step_length = "auto"`) the step length is
the trajectory's mean step between rows, weighted by step length: the
average step over the distance travelled, which time spent still does
not shorten. Sinuosity and E_max describe the path at the scale of that
step, and the automatic step differs between trajectories, so to compare
trajectories, give them all the same `step_length`. Tracking jitter that
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
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -1.14    0.893        0.782
#>  2          1 centroid       1     1     2 -1.44   -0.377        0.682
#>  3          1 centroid       1     1     3 -0.494   0.606        0.623
#>  4          1 centroid       1     1     4  0.841  -0.00487      0.971
#>  5          1 centroid       1     1     5  0.792  -0.521        0.622
#>  6          1 centroid       1     1     6 -0.169  -0.639        0.781
#>  7          1 centroid       1     1     7  0.613  -0.636        0.828
#>  8          1 centroid       1     1     8 -0.771   0.107        0.724
#>  9          1 centroid       1     1     9  0.889   1.18         0.623
#> 10          1 centroid       1     1    10  0.0132  0.447        0.825
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
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -1.14    0.893        0.782
#>  2          1 centroid       1     1     2 -1.44   -0.377        0.682
#>  3          1 centroid       1     1     3 -0.494   0.606        0.623
#>  4          1 centroid       1     1     4  0.841  -0.00487      0.971
#>  5          1 centroid       1     1     5  0.792  -0.521        0.622
#>  6          1 centroid       1     1     6 -0.169  -0.639        0.781
#>  7          1 centroid       1     1     7  0.613  -0.636        0.828
#>  8          1 centroid       1     1     8 -0.771   0.107        0.724
#>  9          1 centroid       1     1     9  0.889   1.18         0.623
#> 10          1 centroid       1     1    10  0.0132  0.447        0.825
#> # ℹ 20 more rows
#> # ℹ 6 more variables: straightness_5 <dbl>, sinuosity_5 <dbl>, e_max_5 <dbl>,
#> #   straightness_11 <dbl>, sinuosity_11 <dbl>, e_max_11 <dbl>

# Sinuosity of every trajectory at one scale, in the frame's spatial unit
data |>
  add_tortuosity(window_width = 11, step_length = 0.5)
#> # Individuals: 1
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -1.14    0.893        0.782
#>  2          1 centroid       1     1     2 -1.44   -0.377        0.682
#>  3          1 centroid       1     1     3 -0.494   0.606        0.623
#>  4          1 centroid       1     1     4  0.841  -0.00487      0.971
#>  5          1 centroid       1     1     5  0.792  -0.521        0.622
#>  6          1 centroid       1     1     6 -0.169  -0.639        0.781
#>  7          1 centroid       1     1     7  0.613  -0.636        0.828
#>  8          1 centroid       1     1     8 -0.771   0.107        0.724
#>  9          1 centroid       1     1     9  0.889   1.18         0.623
#> 10          1 centroid       1     1    10  0.0132  0.447        0.825
#> # ℹ 20 more rows
#> # ℹ 3 more variables: straightness_11 <dbl>, sinuosity_11 <dbl>, e_max_11 <dbl>
```

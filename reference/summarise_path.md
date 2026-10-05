# Summarise each trajectory as a whole

Measures of a path that are not a statistic of any per-row column: how
long it was, where it ended up relative to where it started, and how
directly it got there. Each needs one trajectory per group, since the
distance between the ends of several pooled trajectories describes
nothing.

Positions are all it needs: the kinematics are computed internally, so
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
need not be run first. Any coordinate system works; non-Cartesian frames
are converted to Cartesian for the computation.

For the distribution of per-row measures over each group, such as median
speed or the median of windowed straightness, see
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md).

## Usage

``` r
summarise_path(data, min_step = "auto", step_length = "auto")

summarize_path(data, min_step = "auto", step_length = "auto")
```

## Arguments

- data:

  An anipoint, grouped one trajectory per group, as it is by its
  declared keys.

- min_step:

  **\[experimental\]** The shortest step whose direction counts toward
  `total_turning`, as in
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
  (default `"auto"`). `0` counts every direction, however short the
  step.

- step_length:

  **\[experimental\]** The step length the path is rediscretised at for
  `sinuosity` and `e_max`, in the frame's spatial unit (default
  `"auto"`). `"auto"` takes each trajectory's mean step, weighted by
  step length (see Details). A positive number rediscretises every
  trajectory at that step, so that their sinuosity can be compared at
  one scale.

## Value

A data frame with one row per trajectory:

- `total_distance`: distance travelled, the last value of
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)'s
  `cumulative_distance`.

- `total_turning`: turning of the direction of travel, summed (2D and
  3D), in the frame's `unit_angle`: the last value of
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)'s
  `cumulative_turning`, so steps shorter than `min_step` add none.

- `net_displacement`: straight-line distance from start to end.

- `straightness`: net displacement over distance travelled, from 0 to 1.

- `sinuosity`: corrected sinuosity index (Benhamou 2004), of the path
  rediscretised to a constant step length (see Details).

- `e_max`: maximum expected displacement (dimensionless), from the same
  rediscretised path.

The windowed measures of
[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)
carry the window width in their names (`straightness_11`), so they never
collide with these.

## Details

Sinuosity is defined for a path of constant step length (Benhamou 2004),
so `sinuosity` and `e_max` come from the turning angles of the path
rediscretised to one: walking along the path, a new point is placed
wherever it first leaves a circle of that radius around the last one
(Bovet & Benhamou 1988). By default (`step_length = "auto"`) the step
length is the trajectory's mean step between rows, weighted by step
length: the average step over the distance travelled, which time spent
still does not shorten. Sinuosity and E_max describe the path at the
scale of that step, and the automatic step differs between trajectories:
a faster animal gets a longer one. To compare trajectories, give them
all the same `step_length`. Tracking jitter that stays within the circle
while an animal is still gives no steps and no turning, where turning
angles between successive frames would be dominated by it. Missing
positions break the path into stretches that are rediscretised
separately.

## References

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal's path. Journal of Theoretical Biology, 229(2), 209-220.

Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals' movements
using a correlated random walk model. Journal of Theoretical Biology,
131(4), 419-433.

## Examples

``` r
traj <- anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
summarise_path(traj)
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_distance total_turning
#>        <int> <fct>      <int> <int>          <dbl>         <dbl>
#> 1          1 centroid       1     1           24.3          26.6
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, e_max <dbl>

# Count every direction toward total_turning, however short the step
summarise_path(traj, min_step = 0)
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_distance total_turning
#>        <int> <fct>      <int> <int>          <dbl>         <dbl>
#> 1          1 centroid       1     1           24.3          26.6
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, e_max <dbl>

# Sinuosity at a step of your own, in the frame's spatial unit, the same
# for every trajectory
summarise_path(traj, step_length = 0.5)
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_distance total_turning
#>        <int> <fct>      <int> <int>          <dbl>         <dbl>
#> 1          1 centroid       1     1           24.3          26.6
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, e_max <dbl>
```

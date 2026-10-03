# Summarise each trajectory as a whole

Measures of a path that are not a statistic of any per-row column: how
long it was, where it ended up relative to where it started, and how
directly it got there. Each needs one trajectory per group, since the
distance between the ends of several pooled trajectories describes
nothing.

Positions are all it needs: the kinematics are computed internally, so
[`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
need not be run first. Any coordinate system works; non-Cartesian frames
are converted to Cartesian for the computation.

For the distribution of per-row measures over each group, such as median
speed or the median of windowed straightness, see
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md).

## Usage

``` r
summarise_path(data)

summarize_path(data)
```

## Arguments

- data:

  An anipoint, grouped one trajectory per group, as it is by its
  declared keys.

## Value

A data frame with one row per trajectory:

- `total_path_length`: distance travelled.

- `total_turning`: turning of the direction of travel, summed (2D and
  3D), in the frame's `unit_angle`.

- `net_displacement`: straight-line distance from start to end.

- `straightness`: net displacement over path length, from 0 to 1.

- `sinuosity`: corrected sinuosity index (Benhamou 2004).

- `emax`: maximum expected displacement (dimensionless).

## References

Benhamou, S. (2004). How to reliably estimate the tortuosity of an
animal's path. Journal of Theoretical Biology, 229(2), 209-220.

## Examples

``` r
traj <- anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
summarise_path(traj)
#> # A tibble: 1 × 10
#>   individual keypoint session trial total_path_length total_turning
#>        <int> <fct>      <int> <int>             <dbl>         <dbl>
#> 1          1 centroid       1     1              27.3          29.2
#> # ℹ 4 more variables: net_displacement <dbl>, straightness <dbl>,
#> #   sinuosity <dbl>, emax <dbl>
```

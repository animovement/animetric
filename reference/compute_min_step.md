# The minimum step `"auto"` chooses for each trajectory

**\[experimental\]**

Returns the threshold that `min_step = "auto"` uses in
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
and
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
one per trajectory, with the positional noise it is estimated from. Use
it to see whether the automatic threshold suits your data, to report it,
or to choose a threshold of your own relative to it.

The threshold is computed by the same code that
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
uses, so the two always agree. See the section "The minimum step" in
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
for how it is estimated and when to set it yourself.

## Usage

``` r
compute_min_step(data)
```

## Arguments

- data:

  An anipoint with two or three spatial axes, grouped one trajectory per
  group, as it is by its declared keys. Any coordinate system works;
  non-Cartesian frames are converted to Cartesian for the computation.

## Value

A data frame with one row per trajectory: the key columns, then

- `positional_noise`: the estimated standard deviation of the tracking
  noise on each axis, in the frame's spatial unit. `0` when the
  trajectory is too short, or moves too little, to estimate it.

- `min_step`: the threshold `"auto"` uses, in the frame's spatial unit:
  three times `positional_noise`, or half the trajectory's median step
  if that is smaller. Where it is less than three times
  `positional_noise`, the cap on the median step set it.

## See also

[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md),
whose `min_step` takes the threshold.

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 50, n_individuals = 2, n_keypoints = 1)
compute_min_step(af)
#> # A tibble: 2 × 6
#>   individual keypoint session trial positional_noise min_step
#>        <int> <fct>      <int> <int>            <dbl>    <dbl>
#> 1          1 centroid       1     1            0.780    0.403
#> 2          2 centroid       1     1            1.10     0.409

# One threshold for every trajectory, so their turning is filtered alike
thresholds <- compute_min_step(af)
add_kinematics(af, min_step = max(thresholds$min_step))
#> # Individuals: 1, 2
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x       y confidence speed
#>         <int> <fct>      <int> <int> <int>  <dbl>   <dbl>      <dbl> <dbl>
#>  1          1 centroid       1     1     1 -0.187 -0.870       0.511 1.04 
#>  2          1 centroid       1     1     2 -0.812 -0.0392      0.540 0.748
#>  3          1 centroid       1     1     3 -1.64  -0.518       0.588 0.791
#>  4          1 centroid       1     1     4  0.508 -0.912       0.904 1.73 
#>  5          1 centroid       1     1     5  1.75   0.150       0.754 0.677
#>  6          1 centroid       1     1     6  0.592  0.440       0.621 0.691
#>  7          1 centroid       1     1     7  1.02   1.32        0.806 0.255
#>  8          1 centroid       1     1     8  0.122  0.247       0.822 1.06 
#>  9          1 centroid       1     1     9 -1.08   0.942       0.746 0.698
#> 10          1 centroid       1     1    10 -1.14  -0.342       0.795 0.668
#> # ℹ 90 more rows
#> # ℹ 12 more variables: acceleration <dbl>, cumulative_distance <dbl>,
#> #   v_x <dbl>, v_y <dbl>, a_x <dbl>, a_y <dbl>, course <dbl>,
#> #   course_unwrapped <dbl>, turning_speed <dbl>, turning_rate <dbl>,
#> #   turning_acceleration <dbl>, cumulative_turning <dbl>
```

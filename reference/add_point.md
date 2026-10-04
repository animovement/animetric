# Add a derived point to an anipoint

Derives a new member of an identity level at each moment, from the
members it collapses, and appends it to the frame. The rest of the data
is returned untouched. The centroid of an animal's keypoints is the
usual case; `method` chooses how the point is derived.

Which levels are collapsed is the caller's choice. On pose data for a
team, collapsing `"keypoint"` gives each player a point of their own;
`across = "individual"` gives one point per keypoint across the players;
and collapsing both gives the single point the whole team occupies.

A level that did not actually vary keeps its value rather than taking
the new member's name — an individual's strain is still its strain,
since nothing was derived over it.

The new member is an ordinary member of its level:
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
gives it kinematics, and
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
summarises it alongside the tracked points.

## Usage

``` r
add_point(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL
)
```

## Arguments

- data:

  An anipoint with Cartesian coordinates.

- across:

  Identity variables to collapse — the dimensions the new point is
  derived over. Required when the frame declares more than one identity
  variable, since their order is not a hierarchy and there is no finest
  one to assume; with a single identity variable, that one is the
  default. Collapsing every level gives a single point per position.

- method:

  How each coordinate is derived from the members' values:

  `"centroid"`

  :   The mean.

  `"median"`

  :   The median, per axis: robust to a single stray point.

  `"weighted"`

  :   The mean weighted by `confidence`, so poorly tracked points count
      for less. Needs a `confidence` column.

  a function

  :   Applied to each axis's values, with missing values removed,
      returning one number; e.g. `\(x) mean(x, trim = 0.1)`.

  Missing values are ignored; a moment where every member is missing
  gives `NA`.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed. A midpoint is the centroid of two
  members: `include = c("ear_l", "ear_r")`.

- name:

  Name for the new member. Defaults to `"centroid"`, `"median"` or
  `"weighted_centroid"` after `method`; required when `method` is a
  function.

## Value

The anipoint, with the new member appended as extra rows. The collapsed
identity column comes back as a factor, since it now holds a named
member that an integer column could not.

## Details

A declared orientation is derived too: the circular mean of the members'
`yaw`, in the frame's `unit_angle`, or in 3D the mean of their
quaternions
([`anispace::quat_mean()`](https://animovement.dev/anispace/reference/quat_slerp.html)).
It is weighted by `confidence` when `method` is `"weighted"`, and
unweighted otherwise. The new member has no confidence of its own, so
its `confidence` is `NA`.

## See also

[`compute_point()`](https://animovement.dev/animetric/reference/compute_point.md)
for the new member's rows alone.

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 3)

# Each animal gains a centroid
add_point(af, across = "keypoint")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  2.04    0.0348       0.863
#>  2          1 head           1     1     2  0.449   0.386        0.380
#>  3          1 head           1     1     3  1.39    0.0208       0.442
#>  4          1 head           1     1     4  0.427   0.00759      0.413
#>  5          1 head           1     1     5  0.108   0.931        0.922
#>  6          1 head           1     1     6  0.0223 -0.685        0.714
#>  7          1 head           1     1     7  0.604   0.337        0.730
#>  8          1 head           1     1     8 -0.263  -0.412        0.617
#>  9          1 head           1     1     9 -0.528   0.934        0.688
#> 10          1 head           1     1    10  0.192   1.84         0.854
#> # ℹ 150 more rows

# A median instead, robust to a stray keypoint
add_point(af, across = "keypoint", method = "median")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, median
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  2.04    0.0348       0.863
#>  2          1 head           1     1     2  0.449   0.386        0.380
#>  3          1 head           1     1     3  1.39    0.0208       0.442
#>  4          1 head           1     1     4  0.427   0.00759      0.413
#>  5          1 head           1     1     5  0.108   0.931        0.922
#>  6          1 head           1     1     6  0.0223 -0.685        0.714
#>  7          1 head           1     1     7  0.604   0.337        0.730
#>  8          1 head           1     1     8 -0.263  -0.412        0.617
#>  9          1 head           1     1     9 -0.528   0.934        0.688
#> 10          1 head           1     1    10  0.192   1.84         0.854
#> # ℹ 150 more rows

# The midpoint of two keypoints
add_point(af, across = "keypoint", include = c("head", "neck"), name = "neck_head")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, neck_head
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  2.04    0.0348       0.863
#>  2          1 head           1     1     2  0.449   0.386        0.380
#>  3          1 head           1     1     3  1.39    0.0208       0.442
#>  4          1 head           1     1     4  0.427   0.00759      0.413
#>  5          1 head           1     1     5  0.108   0.931        0.922
#>  6          1 head           1     1     6  0.0223 -0.685        0.714
#>  7          1 head           1     1     7  0.604   0.337        0.730
#>  8          1 head           1     1     8 -0.263  -0.412        0.617
#>  9          1 head           1     1     9 -0.528   0.934        0.688
#> 10          1 head           1     1    10  0.192   1.84         0.854
#> # ℹ 150 more rows

# A custom rule
add_point(af, across = "keypoint", method = \(x) mean(x, trim = 0.1), name = "trimmed")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, trimmed
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1          1 head           1     1     1  2.04    0.0348       0.863
#>  2          1 head           1     1     2  0.449   0.386        0.380
#>  3          1 head           1     1     3  1.39    0.0208       0.442
#>  4          1 head           1     1     4  0.427   0.00759      0.413
#>  5          1 head           1     1     5  0.108   0.931        0.922
#>  6          1 head           1     1     6  0.0223 -0.685        0.714
#>  7          1 head           1     1     7  0.604   0.337        0.730
#>  8          1 head           1     1     8 -0.263  -0.412        0.617
#>  9          1 head           1     1     9 -0.528   0.934        0.688
#> 10          1 head           1     1    10  0.192   1.84         0.854
#> # ℹ 150 more rows

# One point per keypoint, across the animals
add_point(af, across = "individual")
#> # Individuals: 1, 2, centroid
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x        y confidence
#>    <fct>      <fct>      <int> <int> <int>   <dbl>    <dbl>      <dbl>
#>  1 1          head           1     1     1  2.04    0.0348       0.863
#>  2 1          head           1     1     2  0.449   0.386        0.380
#>  3 1          head           1     1     3  1.39    0.0208       0.442
#>  4 1          head           1     1     4  0.427   0.00759      0.413
#>  5 1          head           1     1     5  0.108   0.931        0.922
#>  6 1          head           1     1     6  0.0223 -0.685        0.714
#>  7 1          head           1     1     7  0.604   0.337        0.730
#>  8 1          head           1     1     8 -0.263  -0.412        0.617
#>  9 1          head           1     1     9 -0.528   0.934        0.688
#> 10 1          head           1     1    10  0.192   1.84         0.854
#> # ℹ 170 more rows
```

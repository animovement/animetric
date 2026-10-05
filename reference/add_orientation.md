# Declare orientation from the positions of points

**\[experimental\]**

Works out which way a body faces from where its points are, and declares
it as the frame's orientation: `heading` in 2D, a unit quaternion (`qw`,
`qx`, `qy`, `qz`) in 3D. Once declared, it is a proper `where` variable
that later steps can use, such as egocentric alignment by orientation in
anispace.

The body frame follows anicore's: an orientation of zero faces `+x`.

- Along the body (default):

  `from -> to` is the forward axis, e.g. tail to head.

- Across the body (`perpendicular = TRUE`):

  `from -> to` runs from the body's right to its left, e.g. right eye to
  left eye, and forward is perpendicular to it.

In 3D, two points give a direction but not the roll about it, so a
third, `plane`, is needed. Any point off the line through `from` and
`to` will do: it fixes which way the body's `y` axis points (along the
body), or its forward axis (across the body). A point on the animal's
left makes `y` its left; a point on its back makes `y` point dorsally
instead. Either way the orientation is fully defined; only what its roll
is called depends on the choice.

## Usage

``` r
add_orientation(
  data,
  from,
  to,
  plane = NULL,
  perpendicular = FALSE,
  attach_to = NULL,
  level = NULL,
  name = NULL,
  overwrite = FALSE
)
```

## Arguments

- data:

  A 2D or 3D anipoint with Cartesian coordinates.

- from, to:

  Members of `level` defining the axis: forward, or with
  `perpendicular = TRUE`, right to left.

- plane:

  In 3D, a member of `level` off the line through `from` and `to`,
  fixing the roll about it. Not used in 2D.

- perpendicular:

  Whether `from -> to` runs across the body, from right to left, rather
  than along it.

- attach_to:

  Members of `level` that get the orientation. `NULL`, the default,
  gives it to every member of the subject: the body's orientation. Name
  some to attach different orientations to different parts, e.g. a head
  orientation to the head's points and a body orientation to the rest.

- level:

  The identity variable `from`, `to`, `plane` and `attach_to` name
  members of. Defaults to the frame's only one; a frame declaring
  several has to be told.

- name:

  The orientation column: one name in 2D, four in 3D (`w`, `x`, `y`, `z`
  components). Defaults to the columns of an orientation already
  declared, or else `"heading"` in 2D and `c("qw", "qx", "qy", "qz")` in
  3D.

- overwrite:

  Whether to replace orientation values already present in the rows
  being written, or an orientation declared in other columns.

## Value

The anipoint, with the orientation columns written and declared.

## Details

The orientation is computed for each subject at each moment: each
combination of the frame's other identity variables, its temporal keys
and its index. Where a defining point is missing, or the points
coincide, it is `NA`. Rows not named by `attach_to` keep what they had,
so successive calls can build up orientations for different parts in the
one declared column.

In 2D, `heading` is in the frame's `unit_angle`, counting from the `x`
axis toward the `y` axis, in `(-pi, pi]`. Across the body, forward is
the `from -> to` axis turned a quarter turn, so that `to` is on the
body's left. In 3D the quaternion maps the body's axes into the frame's,
and is built with
[`anispace::quat_from_vectors()`](https://animovement.dev/anispace/reference/quat_from_vectors.html).

## Examples

``` r
af <- anicore::example_anipoint(n_obs = 5, n_individuals = 2, n_keypoints = 3)

# The body faces from the shoulder to the head
add_orientation(af, from = "shoulder_right", to = "head", level = "keypoint")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y heading confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.13  -1.53    -2.10      0.632
#>  2          1 head           1     1     2  0.364  0.237    2.69      0.431
#>  3          1 head           1     1     3 -0.286 -1.31    -2.60      0.574
#>  4          1 head           1     1     4  0.518  0.747    1.88      0.793
#>  5          1 head           1     1     5 -0.103 -1.56    -2.56      0.368
#>  6          1 neck           1     1     1 -0.474 -1.69    -2.10      0.826
#>  7          1 neck           1     1     2 -1.28  -0.903    2.69      0.966
#>  8          1 neck           1     1     3 -0.306  1.32    -2.60      0.893
#>  9          1 neck           1     1     4  2.21   1.10     1.88      0.778
#> 10          1 neck           1     1     5 -1.04   1.20    -2.56      0.529
#> # ℹ 20 more rows

# Attached to the head only
add_orientation(
  af,
  from = "shoulder_right",
  to = "head",
  attach_to = "head",
  level = "keypoint"
)
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time      x      y heading confidence
#>         <int> <fct>      <int> <int> <int>  <dbl>  <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.13  -1.53    -2.10      0.632
#>  2          1 head           1     1     2  0.364  0.237    2.69      0.431
#>  3          1 head           1     1     3 -0.286 -1.31    -2.60      0.574
#>  4          1 head           1     1     4  0.518  0.747    1.88      0.793
#>  5          1 head           1     1     5 -0.103 -1.56    -2.56      0.368
#>  6          1 neck           1     1     1 -0.474 -1.69    NA         0.826
#>  7          1 neck           1     1     2 -1.28  -0.903   NA         0.966
#>  8          1 neck           1     1     3 -0.306  1.32    NA         0.893
#>  9          1 neck           1     1     4  2.21   1.10    NA         0.778
#> 10          1 neck           1     1     5 -1.04   1.20    NA         0.529
#> # ℹ 20 more rows
```

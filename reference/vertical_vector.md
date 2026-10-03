# The unit vertical of a frame

The unit vertical of a frame

## Usage

``` r
vertical_vector(n_axes, vertical)
```

## Arguments

- n_axes:

  How many Cartesian axes the frame has.

- vertical:

  See
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  already checked.

## Value

A length-3 unit vector, or `NULL` when a 3D frame has no `vertical`. In
2D it is the normal of the data's plane.

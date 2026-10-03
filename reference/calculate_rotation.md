# Calculate how the direction of travel changes

Computes the turning measures of the path from the velocity vectors, for
2D and 3D data. A 2D vector is treated as lying in the horizontal plane
of a 3D one, so both share one computation. The angles are computed in
radians and returned in the frame's `unit_angle`.

## Usage

``` r
calculate_rotation(data, vertical = NULL)
```

## Arguments

- data:

  A 2D or 3D Cartesian anipoint with velocity (`v_*`) columns, and an
  index.

- vertical:

  See
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md).

## Value

The anipoint with added rotational kinematic columns.

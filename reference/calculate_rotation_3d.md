# Calculate rotational kinematics in 3D

Computes 3D orientation angles and angular kinematics based on the
velocity vector. Uses spherical coordinates: azimuth (horizontal angle)
and elevation (vertical angle).

## Usage

``` r
calculate_rotation_3d(data)
```

## Arguments

- data:

  An anipoint with v_x, v_y, v_z, and time columns

## Value

The anipoint with added rotational kinematic columns

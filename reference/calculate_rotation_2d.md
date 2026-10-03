# Calculate rotational kinematics in 2D

Computes the course (direction of travel) and turning measures from the
velocity vector. Course is calculated as atan2(v_y, v_x), and is `NA`
where speed is zero. The angles are computed in radians and returned in
the frame's `unit_angle`.

## Usage

``` r
calculate_rotation_2d(data)
```

## Arguments

- data:

  An anipoint with v_x, v_y and speed columns, and an index

## Value

The anipoint with added rotational kinematic columns

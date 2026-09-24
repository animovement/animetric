# Calculate rotational kinematics in 2D

Computes heading angles and angular kinematics based on the velocity
vector. Heading is calculated as atan2(v_y, v_x).

## Usage

``` r
calculate_rotation_2d(data)
```

## Arguments

- data:

  An anipoint with v_x, v_y, and time columns

## Value

The anipoint with added rotational kinematic columns

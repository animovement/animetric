# Calculate kinematic measures from trajectory data

Computes translational and rotational kinematic measures from movement
data. Handles data in any coordinate system by automatically converting
to Cartesian for calculations, then converting back to the original
system.

## Usage

``` r
calculate_kinematics(data)

calculate_kinematics_2d(data)

calculate_kinematics_3d(data)
```

## Arguments

- data:

  An aniframe with position coordinates (x/y or x/y/z for Cartesian;
  rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
  spherical) and a time column

## Value

An aniframe in the same coordinate system as the input, with added
kinematic measures. For 2D data, includes translational kinematics
(velocity components, speed, acceleration, path length) and rotational
kinematics (heading, angular velocity, angular speed, angular
acceleration). For 3D data, includes translational kinematics only
(rotational measures for 3D are not yet implemented).

## Details

The function preserves the original coordinate system by:

1.  Detecting the input coordinate system from metadata

2.  Converting to Cartesian if necessary

3.  Computing kinematics in Cartesian space

4.  Converting back to the original coordinate system

All kinematic calculations are performed using numerical differentiation
via the `differentiate` function. Angles are unwrapped to handle
discontinuities at ±π.

## Examples

``` r
if (FALSE) { # \dontrun{
# 2D Cartesian data
traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
  as_aniframe()
kinematics_2d <- calculate_kinematics(traj_2d)

# Polar data (automatically converted and converted back)
traj_polar <- aniframe::map_to_polar(traj_2d)
kinematics_polar <- calculate_kinematics(traj_polar)
} # }
```

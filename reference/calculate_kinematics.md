# Calculate kinematic measures from trajectory data

Computes translational and rotational kinematic measures from movement
data. Handles data in any coordinate system by automatically converting
to Cartesian for calculations, then converting back to the original
system.

## Usage

``` r
calculate_kinematics(data)
```

## Arguments

- data:

  An anipoint with position coordinates (x, x/y or x/y/z for Cartesian;
  rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
  spherical) and an index. The axes and the index are read from the
  frame's declared variables, so the columns can have any name.

## Value

An anipoint in the same coordinate system as the input, with added
kinematic measures. Translational kinematics (velocity and acceleration
components, speed, acceleration, path length) are computed for 1D, 2D
and 3D data, with components named by axis role (`v_x`, `v_y`, ...)
whatever the input columns are called. For 2D data, measures of the
path's direction are added too:

- `course`:

  Direction of travel, `atan2(v_y, v_x)`. It is `NA` where speed is
  zero, since a stationary animal has no direction of travel.
  `course_unwrapped` is the same, without jumps at +/-pi.

- `turning_rate`:

  Rate of change of the course: signed curvature times speed.

- `turning_speed`:

  Absolute turning rate.

- `turning_acceleration`:

  Rate of change of the turning rate.

- `cumulative_turning`:

  Absolute turning accumulated since the first row.

These describe the path, not the body: course is where the animal is
going, not where it is facing, and the two differ for any animal that
does not move nose-first. The names heading and angular velocity are
kept for body orientation. Measures of the path's direction for 1D and
3D are not yet implemented.

Angular measures are in the frame's declared `unit_angle`, radians or
degrees; turning rate and acceleration are per unit of the index. Signed
angles (course, turning rate) follow the frame's own convention: course
counts from the `x` axis toward the `y` axis, which is counter-clockwise
when
[`anicore::get_angle_direction()`](https://animovement.dev/anicore/reference/get_angle_direction.html)
says so (`y` pointing up, as aniread leaves image data) and clockwise in
a frame whose `y` points down. To change convention, change the
coordinates, for example with
[`anicore::reflect_axis()`](https://animovement.dev/anicore/reference/reflect_axis.html),
and the angles follow.

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
# 2D Cartesian data
traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
  anicore::as_anipoint()
kinematics_2d <- calculate_kinematics(traj_2d)

# Polar data (automatically converted and converted back)
traj_polar <- anispace::map_to_polar(traj_2d)
kinematics_polar <- calculate_kinematics(traj_polar)
```

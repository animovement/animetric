# Calculate kinematic measures from trajectory data

Computes translational and rotational kinematic measures from movement
data. Handles data in any coordinate system by automatically converting
to Cartesian for calculations, then converting back to the original
system.

## Usage

``` r
calculate_kinematics(data, vertical = NULL)
```

## Arguments

- data:

  An anipoint with position coordinates (x, x/y or x/y/z for Cartesian;
  rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
  spherical) and an index. The axes and the index are read from the
  frame's declared variables, so the columns can have any name.

- vertical:

  For 3D data, the axis that points up in the world, against gravity:
  one of `"x"`, `"y"` or `"z"`, or with a minus sign (`"-y"`) when that
  axis points down. It defines the horizontal plane that course is
  measured in. The frame's `axis_directions` cannot supply it, since
  they are relative to the camera: in a recording filmed from above, the
  axis pointing at the camera is the vertical one. A metadata field for
  it is proposed in animovement/anicore#172. `NULL` (the default) gives
  only the measures that need no vertical. Ignored for 1D and 2D data,
  where course is measured in the plane of the data.

## Value

An anipoint in the same coordinate system as the input, with added
kinematic measures. Translational kinematics (velocity and acceleration
components, speed, acceleration, path length) are computed for 1D, 2D
and 3D data, with components named by axis role (`v_x`, `v_y`, ...)
whatever the input columns are called. For 2D and 3D data, measures of
how the direction of travel changes are added too:

- `turning_speed`:

  How fast the direction of travel turns, in any direction: curvature
  times speed. In 2D it is the size of the turning rate. In 3D it also
  counts climbing and diving, and is the angle between the velocities
  either side of each row, over the time between them.

- `cumulative_turning`:

  Turning accumulated since the first row: the angle between successive
  velocities, summed. A turn made while stationary is counted when the
  animal moves off.

In 2D, and in 3D when `vertical` is given, the measures that need a
direction to count from:

- `course`:

  Direction of travel in the horizontal plane (in 2D, the plane of the
  data): `atan2(v_y, v_x)` in 2D. It is `NA` where there is no
  horizontal movement, since the direction is then undefined.
  `course_unwrapped` is the same, without jumps at +/-pi.

- `course_elevation`:

  3D only: the angle of travel above the horizontal plane, from -pi/2
  (straight down) to pi/2 (straight up).

- `turning_rate`:

  Signed rate of change of the course, the derivative of
  `course_unwrapped`. In 3D it counts horizontal turning only.

- `turning_acceleration`:

  Rate of change of the turning rate.

These describe the path, not the body: course is where the animal is
going, not where it is facing, and the two differ for any animal that
does not move nose-first. The names heading and angular velocity are
kept for body orientation. 1D data gets translational measures only.

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
and the angles follow. In 3D, course counts about `vertical` by the
right-hand rule, from the next axis in the cycle x, y, z: from `x`
toward `y` when `z` is vertical, from `z` toward `x` when `y` is, from
`y` toward `z` when `x` is.

## Details

The function preserves the original coordinate system by:

1.  Detecting the input coordinate system from metadata

2.  Converting to Cartesian if necessary

3.  Computing kinematics in Cartesian space

4.  Converting back to the original coordinate system

All kinematic calculations are performed using numerical differentiation
via the `differentiate` function. Course is unwrapped before it is
differentiated, so the turning rate has no jump at +/-pi. The turning
speed in 3D needs no angle to unwrap, and has no singularity when travel
is vertical.

## Examples

``` r
# 2D Cartesian data
traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
  anicore::as_anipoint()
kinematics_2d <- calculate_kinematics(traj_2d)

# Polar data (automatically converted and converted back)
traj_polar <- anispace::map_to_polar(traj_2d)
kinematics_polar <- calculate_kinematics(traj_polar)

# 3D data with z pointing up: course and elevation of travel
traj_3d <- data.frame(
  time = 0:10,
  x = cos(0:10 / 2),
  y = sin(0:10 / 2),
  z = 0:10 / 10
) |>
  anicore::as_anipoint()
kinematics_3d <- calculate_kinematics(traj_3d, vertical = "z")
```

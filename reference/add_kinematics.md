# Add kinematic measures to trajectory data

Computes translational and rotational kinematic measures from movement
data, and returns the frame with a column for each. Handles data in any
coordinate system by automatically converting to Cartesian for the
calculations, then converting back to the original system.

## Usage

``` r
add_kinematics(data, vertical = NULL, min_step = "auto")
```

## Arguments

- data:

  An anipoint with position coordinates (x, x/y or x/y/z for Cartesian;
  rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
  spherical) and an index. The axes and the index are read from the
  frame's declared variables, so the columns can have any name.

- vertical:

  **\[experimental\]** For 3D data, the axis that points up in the
  world, against gravity: one of `"x"`, `"y"` or `"z"`, or with a minus
  sign (`"-y"`) when that axis points down. It defines the horizontal
  plane that course is measured in. The frame's `axis_directions` cannot
  supply it, since they are relative to the camera: in a recording
  filmed from above, the axis pointing at the camera is the vertical
  one. A metadata field for it is proposed in animovement/anicore#172.
  `NULL` (the default) gives only the measures that need no vertical.
  Ignored for 1D and 2D data, where course is measured in the plane of
  the data. Experimental until that proposal is settled: the argument
  may come to default to the declared vertical, and to mean something
  for 2D side views.

- min_step:

  **\[experimental\]** The shortest step, in the frame's spatial unit,
  whose direction counts. Where a point moves less than this per sample,
  its direction of travel is set by tracking noise rather than by where
  the animal is going, so the direction is treated as undefined:
  `course` is `NA` there, and the row adds no turning. One of:

  `"auto"` (the default)

  :   Three times the tracking noise, estimated separately for each
      trajectory, and at most half its median step. See Details.

  a number

  :   A threshold of your own, such as the tracking precision in the
      frame's spatial unit.

  `0`

  :   Every direction counts, however short the step, as
      [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
      did.

  It affects only the direction measures (`course` and the turning
  measures), not `speed` or `cumulative_distance`.

## Value

An anipoint in the same coordinate system as the input, with added
kinematic measures. Translational measures are computed for 1D, 2D and
3D data:

- `v_x`, `v_y`, `v_z`:

  Velocity components, named by axis role whatever the input columns are
  called.

- `a_x`, `a_y`, `a_z`:

  Acceleration components, the second derivatives of the positions.

- `speed`:

  The size of the velocity.

- `acceleration`:

  The signed rate of change of `speed`: positive when speeding up,
  negative when slowing down. This is the tangential acceleration, along
  the path. It is not the size of the acceleration vector (`a_x`,
  `a_y`), which also includes the part that turns the path: an animal
  rounding a corner at constant speed has an `acceleration` of 0 but
  non-zero `a_x` and `a_y`.

- `cumulative_distance`:

  Distance travelled since the first row: the straight-line steps
  between successive positions, summed. A step to or from a missing
  position adds nothing.

For 2D and 3D data, measures of how the direction of travel changes are
added too:

- `turning_speed`:

  How fast the direction of travel turns, in any direction: curvature
  times speed. In 2D it is the size of the turning rate. In 3D it also
  counts climbing and diving, and is the angle between the velocities
  either side of each row, over the time between them.

- `cumulative_turning`:

  Turning accumulated since the first row: the angle between successive
  velocities, summed. A turn made while stationary, or while moving less
  than `min_step`, is counted when the animal moves off, as the angle
  between the directions before and after.

In 2D, and in 3D when `vertical` is given, the measures that need a
direction to count from:

- `course`:

  Direction of travel in the horizontal plane (in 2D, the plane of the
  data): `atan2(v_y, v_x)` in 2D. It is `NA` where there is no
  horizontal movement, or less than `min_step` per sample, since the
  direction is then undefined. `course_unwrapped` is the same, without
  jumps at +/-pi.

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

### The minimum step

When a point barely moves, the direction between successive positions is
set by sub-pixel tracking noise, and swings at random from frame to
frame. Every swing is turning to the measures above, so a resting animal
accumulates turning as fast as a running one. `min_step` sets the step
below which the direction is not trusted.

The step of a row is the distance its velocity implies moving per
sample: `speed` times the sampling interval, which for the central
differences used here is half the distance between the positions either
side. Course is measured against the horizontal part of the step alone.

With `min_step = "auto"`, the threshold is three times the positional
noise, \\\sigma\\, estimated for each trajectory from its second
differences \\p\_{i+1} - 2 p_i + p\_{i-1}\\. For white noise of standard
deviation \\\sigma\\ on each axis these have standard deviation
\\\sqrt{6}\sigma\\. Only their component along the direction of travel
is used, which a path turning at constant speed leaves at zero, and
their spread is measured with
[`stats::mad()`](https://rdrr.io/r/stats/mad.html), which a minority of
large values barely moves. A point standing still under white noise
makes steps longer than \\3\sigma\\ with a probability of about 0.01%
(0.04% in 3D), so the threshold removes the directions of a stationary
point while keeping those of real movement.

The threshold is also at most half the trajectory's median step (among
steps that have a direction), so most steps always keep theirs. This
matters when the sampling is coarse for the movement: a random walk
recorded step by step has second differences as large as its steps, and
the estimate would take the movement itself for noise.

The estimate assumes the sampling is fast compared with changes in
speed, as it is for video tracking. Two cases call for setting
`min_step` yourself:

- **Smoothed positions.** Smoothing turns noise into slow wander, which
  second differences see less of, so the estimate is smaller than the
  jitter left in the data, and the threshold removes only part of it.

- **Coarse sampling**, such as GPS fixes minutes apart, where speed
  changes a lot between samples. The estimate then includes real
  movement, so the threshold may be too large; use the positional
  precision instead, or `0`.

The threshold is not stored in the result. To know it exactly, give it
as a number.

## See also

[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)
for windowed measures of how winding the path is, and
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
for measures of each whole trajectory.

## Examples

``` r
# 2D Cartesian data
traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
  anicore::as_anipoint()
kinematics_2d <- add_kinematics(traj_2d)

# Polar data (automatically converted and converted back)
traj_polar <- anispace::map_to_polar(traj_2d)
kinematics_polar <- add_kinematics(traj_polar)

# 3D data with z pointing up: course and elevation of travel
traj_3d <- data.frame(
  time = 0:10,
  x = cos(0:10 / 2),
  y = sin(0:10 / 2),
  z = 0:10 / 10
) |>
  anicore::as_anipoint()
kinematics_3d <- add_kinematics(traj_3d, vertical = "z")

# A threshold of your own, in the frame's spatial unit, or 0 to count
# every direction however short the step
kinematics_2d <- add_kinematics(traj_2d, min_step = 0.1)
```

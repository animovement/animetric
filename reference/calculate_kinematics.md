# Calculate kinematic measures from trajectory data

**\[deprecated\]**

Renamed to
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md),
which takes the same arguments: functions that return the frame with
columns added now start with `add_`. This returns exactly what it did,
including the running total of distance as `path_length`, which
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
calls `cumulative_distance`, and every direction however short the step,
as
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
does with `min_step = 0`.

## Usage

``` r
calculate_kinematics(data, vertical = NULL)
```

## Arguments

- data, vertical:

  See
  [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md).

## Value

As
[`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md),
with `path_length` in place of `cumulative_distance`.

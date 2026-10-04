# Calculate tortuosity metrics over sliding windows

**\[deprecated\]**

Renamed to
[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md),
which takes the same arguments. This returns exactly what it did:
columns `straightness`, `sinuosity` and `emax`, plus every kinematic
column of
[`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
when the frame did not already have velocities and speed.
[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)
adds only its three measures, with the window width in their names
(`straightness_11`, `sinuosity_11`, `e_max_11`).

## Usage

``` r
calculate_tortuosity(data, window_width = 11L)
```

## Arguments

- data:

  A Cartesian anipoint.

- window_width:

  Size of the sliding window, in observations (default `11L`). Should be
  an odd number \>= 3 for symmetric centering.

## Value

The input anipoint with `straightness`, `sinuosity` and `emax` added,
and the kinematic columns if they were missing.

# Convert angles between radians and a frame's angular unit

The angular computations work in radians. `rad_to_unit()` expresses
their results in the frame's unit, and `unit_to_rad()` reads the frame's
angles back into radians.

## Usage

``` r
rad_to_unit(x, unit)

unit_to_rad(x, unit)
```

## Arguments

- x:

  Numeric vector of angles, or of angular rates.

- unit:

  `"rad"` or `"deg"`, as from
  [`angle_unit()`](https://animovement.dev/animetric/reference/angle_unit.md).

## Value

Numeric vector.

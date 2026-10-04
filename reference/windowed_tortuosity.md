# Straightness, sinuosity and E_max over sliding windows, as `calculate_tortuosity()` computed them

Turning angles are between successive velocities, frame by frame.

## Usage

``` r
windowed_tortuosity(data, window_width, v_cols, names)
```

## Arguments

- data:

  A Cartesian anipoint with velocity columns.

- window_width:

  The window width, an integer of at least 3.

- v_cols:

  The velocity columns, one per axis. Columns whose names start with `.`
  are dropped from the result.

- names:

  The names to give straightness, sinuosity and E_max.

## Value

The anipoint with the three measures added.

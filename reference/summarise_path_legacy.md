# What `summarise_tortuosity()` returned

Every direction counts toward `total_turning`, and sinuosity and E_max
come from the turning between successive frames.

## Usage

``` r
summarise_path_legacy(data)
```

## Arguments

- data:

  An anipoint.

## Value

As
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
with `total_path_length` in place of `total_distance` and `emax` in
place of `e_max`.

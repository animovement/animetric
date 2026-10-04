# Calculate tortuosity summary statistics

**\[deprecated\]**

Renamed to
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
which returns the same measures. The name suggested a summary of
[`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)'s
windowed output, which it never was: it measures the whole path. To
summarise the windowed measures, use
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md).

This returns exactly what it did, with the columns `total_path_length`
and `emax`, which
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
calls `total_distance` and `e_max`.

## Usage

``` r
summarise_tortuosity(data)

summarize_tortuosity(data)
```

## Arguments

- data:

  An anipoint.

## Value

As
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
with `total_path_length` and `emax` in place of `total_distance` and
`e_max`.

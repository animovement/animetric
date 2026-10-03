# Calculate tortuosity summary statistics

**\[deprecated\]**

Renamed to
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
which returns the same measures. The name suggested a summary of
[`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)'s
windowed output, which it never was: it measures the whole path. To
summarise the windowed measures, use
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md).

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
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md).

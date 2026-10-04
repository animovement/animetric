# The columns [`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md) has added to a frame

Its columns are named `<measure>_<window width>`, one set per width, so
they are found by that pattern rather than by a fixed list.

## Usage

``` r
tortuosity_columns(data)
```

## Arguments

- data:

  A data frame.

## Value

The names of the windowed tortuosity columns, in frame order.

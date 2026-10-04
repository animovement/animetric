# Turning along a rediscretised path

Turning along a rediscretised path

## Usage

``` r
rediscretised_turning(path)
```

## Arguments

- path:

  A rediscretised path, from
  [`rediscretise_path()`](https://animovement.dev/animetric/reference/rediscretise_path.md).

## Value

A data frame with one row per point between two steps of the same
stretch: `time`, when the path reaches the point, and `cos_turning`, the
cosine of the angle between the steps either side of it.

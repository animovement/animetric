# The time each row's velocity spans, per sample

Velocity is a central difference, so the distance it implies moving per
sample is speed times half the time between the rows either side: half
the distance between those two positions. The ends use one-sided
differences, as
[`differentiate()`](https://animovement.dev/animetric/reference/differentiate.md)
does.

## Usage

``` r
sampling_interval(time)
```

## Arguments

- time:

  The index.

## Value

Numeric vector, in units of the index.

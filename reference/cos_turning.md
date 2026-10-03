# Cosine of the turning angle between successive velocity vectors

The angle between each row's velocity and the previous row's, from
[`anicore::angle_between()`](https://animovement.dev/anicore/reference/angle_between.html).
It is `NA` in the first row and wherever either velocity is zero or
missing. A 1D velocity is treated as lying in a plane, so a reversal is
a turn of `pi`.

## Usage

``` r
cos_turning(velocity)
```

## Arguments

- velocity:

  A list or data frame of velocity components, one per axis.

## Value

Numeric vector.

# Test whether a frame holds kinematics

**\[deprecated\]**

The `aniframe_kin` class is retired: it only labelled a frame as having
been through
[`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
said nothing about which columns it held, and outlived them
(`select(-speed)` kept it). Check for the columns you need instead, e.g.
`"speed" %in% names(x)`.

## Usage

``` r
is_aniframe_kin(x)
```

## Arguments

- x:

  An object.

## Value

`TRUE` for an anipoint with a `speed` column.

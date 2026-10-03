# The mean of a set of unit quaternions

The mean of a set of unit quaternions

## Usage

``` r
mean_quaternion(q, w = NULL)
```

## Arguments

- q:

  A data frame of quaternion components, `qw, qx, qy, qz` in that order,
  one row per member.

- w:

  Weights, or `NULL` for equal ones.

## Value

A one-row data frame with the same columns: the mean quaternion from
[`anispace::quat_mean()`](https://animovement.dev/anispace/reference/quat_slerp.html),
or `NA` when no member has one.

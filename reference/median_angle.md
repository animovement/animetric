# Compute the circular median of angles

Returns the median direction of a vector of angles in radians, wrapped
to \[0, 2\*pi).

## Usage

``` r
median_angle(ang)
```

## Arguments

- ang:

  Numeric vector of angles in radians.

## Value

Circular median in \[0, 2\*pi).

## Examples

``` r
median_angle(c(pi/2, 1.5 * pi))
#> [1] 3.141593
median_angle(c(0, pi))
#> [1] 1.570796
```

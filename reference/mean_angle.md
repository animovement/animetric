# Compute the circular mean of angles

Returns the mean direction of a vector of angles in radians, wrapped to
\[0, 2\*pi).

## Usage

``` r
mean_angle(ang)
```

## Arguments

- ang:

  Numeric vector of angles in radians.

## Value

Circular mean in \[0, 2\*pi).

## Examples

``` r
mean_angle(c(pi/2, 1.5 * pi))
#> [1] 3.141593
mean_angle(c(0, pi))
#> [1] 1.570796
```

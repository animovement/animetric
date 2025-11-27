# Compute the circular median of a set of angles (radians)

The function returns the mean direction of a numeric vector of angles,
wrapping the result to the interval \[0, 2\*pi). It leverages the
[`wrap_angle()`](http://animovement.dev/aniframe/reference/wrap_angle.md)
and
[`calculate_angular_difference()`](http://animovement.dev/aniframe/reference/calculate_angular_difference.md)
helpers that you already defined.

## Usage

``` r
median_angle(ang)
```

## Arguments

- ang:

  Numeric vector of angles (radians). May contain values outside \[0,
  2\*pi); they are normalised internally.

## Value

A single numeric value in \[0, 2\*pi) representing the circular mean.

## Examples

``` r
median_angle(c(pi/2, 1.5 * pi))   # → 0
#> [1] 3.141593
median_angle(c(0, pi))           # → pi
#> [1] 1.570796
```

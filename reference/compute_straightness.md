# Compute straightness index from precomputed vectors

Compute straightness index from precomputed vectors

## Usage

``` r
compute_straightness(displacement, path_length)
```

## Arguments

- displacement:

  Numeric vector of net displacements (D)

- path_length:

  Numeric vector of path lengths (L)

## Value

Numeric vector of straightness values (D/L)

## Examples

``` r
# Straight-line displacement over the distance actually travelled
compute_straightness(5, 10)
#> [1] 0.5

# A perfectly straight path scores 1
compute_straightness(10, 10)
#> [1] 1
```

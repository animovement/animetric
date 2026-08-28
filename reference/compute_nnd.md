# Compute nearest neighbour distances within one group

Low-level function behind
[`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md),
operating on plain vectors for one group of comparable points (typically
one timepoint).

## Usage

``` r
compute_nnd(
  x,
  y,
  z = NULL,
  across,
  n = 1L,
  is_focal = NULL,
  is_candidate = NULL,
  labels = NULL
)
```

## Arguments

- x, y:

  Numeric vectors of coordinates.

- z:

  Numeric vector of coordinates, or `NULL` for 2D data.

- across:

  Vector whose value must differ between a focal point and its neighbour
  — the thing being ranked, e.g. individual identity.

- n:

  Which neighbour to return (1 = nearest, 2 = second nearest).

- is_focal:

  Logical vector marking which points to measure from, or `NULL` for all
  of them. Non-focal points get `NA` results.

- is_candidate:

  Logical vector marking which points may be returned as a neighbour, or
  `NULL` for all of them.

- labels:

  Named list of vectors describing each point. The matched neighbour's
  value is reported for each, as `nnd_<name>`.

## Value

A tibble with `nnd_across` (the value of `across` for the matched
neighbour), one `nnd_<name>` column per entry of `labels`, and
`nnd_distance`.

## Details

For each focal point, candidates are ranked by the value of `across`:
the closest candidate point is found for each distinct value, those
values are ranked by that distance, and the `n`-th is returned. So
"second nearest" means the second nearest *entity*, not the second
nearest point.

## See also

[`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
for the aniframe-level function.

## Examples

``` r
# Nearest point belonging to a different individual
compute_nnd(
  x = c(0, 1, 2),
  y = c(0, 1, 0),
  across = c("a", "b", "c")
)
#> # A tibble: 3 × 2
#>   nnd_across nnd_distance
#>   <chr>             <dbl>
#> 1 b                  1.41
#> 2 a                  1.41
#> 3 b                  1.41

# The second nearest, with the neighbour's label reported back
compute_nnd(
  x = c(0, 1, 2),
  y = c(0, 1, 0),
  across = c("a", "b", "c"),
  n = 2L,
  labels = list(individual = c("a", "b", "c"))
)
#> # A tibble: 3 × 3
#>   nnd_across nnd_individual nnd_distance
#>   <chr>      <chr>                 <dbl>
#> 1 c          c                      2   
#> 2 c          c                      1.41
#> 3 a          a                      2   
```

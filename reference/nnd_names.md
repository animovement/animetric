# Name the columns `add_nnd()` adds

`nnd_`, the neighbour rank, then what the column holds, so the names
split with `"^nnd_(\\d+)_(.+)$"`. In the order
[`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
returns its columns: which entity the neighbour belongs to, its value
for each unconstrained identity variable, and the distance.

## Usage

``` r
nnd_names(across, unconstrained, n)
```

## Arguments

- across, n:

  As in
  [`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md).

- unconstrained:

  The identity variables neither `across` nor in the context.

## Value

`nnd_<n>_<across>`, `nnd_<n>_<variable>` for each of `unconstrained`,
and `nnd_<n>_distance`.

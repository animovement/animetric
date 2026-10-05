# Calculate distance to the n-th nearest neighbour

**\[deprecated\]**

Renamed to
[`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md),
which takes the same arguments apart from `keypoint_neighbour`:
functions that return the frame with columns added now start with
`add_`. This returns exactly what it did, with the columns named without
the neighbour rank that
[`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md)
puts in them: `nnd_<across>` and `nnd_distance` rather than
`nnd_<n>_<across>` and `nnd_<n>_distance`.

## Usage

``` r
calculate_nnd(
  data,
  across,
  n = 1L,
  within = NULL,
  focal = NULL,
  neighbour = NULL,
  keypoint_neighbour = NULL
)
```

## Arguments

- data, across, n, within, focal, neighbour:

  See
  [`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md).

- keypoint_neighbour:

  Deprecated. Use `neighbour = list(keypoint = ...)`.

## Value

As
[`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md),
with the columns it adds named `nnd_distance`, `nnd_<across>` and
`nnd_<variable>`.

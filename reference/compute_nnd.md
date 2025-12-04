# Compute nearest neighbour distances for a single time point

Low-level function that computes distances to the nth nearest
individual. For each focal point, finds the closest point belonging to
the nth nearest individual (ranked by minimum distance). Called by
[`calculate_nnd()`](http://animovement.dev/animetric/reference/calculate_nnd.md)
for each time point.

## Usage

``` r
compute_nnd(
  x,
  y,
  z = NULL,
  individual,
  keypoint = NULL,
  n = 1L,
  keypoint_neighbour = NULL
)
```

## Arguments

- x:

  Numeric vector of x coordinates.

- y:

  Numeric vector of y coordinates.

- z:

  Numeric vector of z coordinates, or NULL for 2D data.

- individual:

  Factor or vector identifying which individual each point belongs to.

- keypoint:

  Factor or vector identifying keypoint labels, or NULL if no keypoints.

- n:

  Which individual to find (1 = nearest, 2 = second nearest, etc.).

- keypoint_neighbour:

  Character vector of keypoint(s) to consider as valid neighbours, or
  NULL to consider all.

## Value

A tibble with columns:

- `nnd_individual`: individual ID of the n-th nearest individual

- `nnd_keypoint`: keypoint of the closest point on that individual (only
  if `keypoint` is not NULL)

- `nnd_distance`: distance to the closest point on the n-th nearest
  individual

## See also

[`calculate_nnd()`](http://animovement.dev/animetric/reference/calculate_nnd.md)
for the user-facing aniframe function

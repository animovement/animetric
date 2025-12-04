# Calculate distance to n-th nearest neighbour

Computes the distance from each point to the n-th nearest point
belonging to a different individual. Optionally filter by keypoint on
neighbouring individuals.

## Usage

``` r
calculate_nnd(data, n = 1L, keypoint_neighbour = NULL)
```

## Arguments

- data:

  An aniframe with x, y (and optionally z) coordinates and individual
  identifiers.

- n:

  Which neighbour (1 = nearest, 2 = second nearest, etc.).

- keypoint_neighbour:

  Keypoint(s) on other individuals to consider as potential neighbours.
  Can be a single keypoint or a character vector. If NULL (default),
  considers all keypoints.

## Value

The input aniframe with additional columns:

- `nnd_distance`: distance to the n-th nearest neighbour

- `nnd_individual`: individual ID of the n-th nearest neighbour

- `nnd_keypoint`: keypoint of the neighbour (if keypoint column exists)

## Examples

``` r
if (FALSE) { # \dontrun{
# Distance to nearest individual (any keypoint to any keypoint)
data |> calculate_nnd()

# Distance to nearest nose of another individual
data |> calculate_nnd(keypoint_neighbour = "nose")

# Nose-to-nose distance
data |>
  calculate_nnd(keypoint_neighbour = "nose") |>
  dplyr::filter(keypoint == "nose")

# Minimum distance between individuals (across all keypoints)
data |>
  calculate_nnd() |>
  dplyr::group_by(session, trial, time, individual) |>
  dplyr::slice_min(nnd_distance, n = 1)
} # }
```

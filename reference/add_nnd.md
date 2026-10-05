# Add the distance to the n-th nearest neighbour

Computes, for each point, the distance to the nearest point belonging to
a *different* entity — typically a different individual at the same
moment.

Which columns carry time and position is read from the anipoint's
declared variables (see
[`anicore::get_variables()`](https://animovement.dev/anicore/reference/variables.html)).
The identity columns are assigned roles by you, explicitly, because
"another animal" and "another point on this animal" are different
questions and the data cannot tell which one you mean.

## Usage

``` r
add_nnd(data, across, n = 1L, within = NULL, focal = NULL, neighbour = NULL)
```

## Arguments

- data:

  An anipoint.

- across:

  Column whose value must differ between a point and its neighbour.

- n:

  Which neighbour to return (1 = nearest, 2 = second nearest). Ranked by
  entity, not by point: with `n = 2`, the result is the closest point on
  the second-nearest entity.

- within:

  Identity columns that must match, added to the temporal context.

- focal:

  Named list restricting which points are measured from, e.g.
  `list(keypoint = "nose")`. `NULL` measures from every point.

- neighbour:

  Named list restricting which points may be returned as a neighbour,
  e.g. `list(keypoint = "tail")`.

## Value

The input anipoint with added columns, each named `nnd_`, then the
neighbour rank `n`, then what the column holds:

- `nnd_<n>_<across>` — which entity the n-th nearest neighbour belongs
  to (e.g. `nnd_1_individual`)

- `nnd_<n>_<variable>` — the neighbour's value for each unconstrained
  identity variable (e.g. `nnd_1_keypoint`)

- `nnd_<n>_distance` — the distance to it

The rank is always there, `1` included, so the nearest and
second-nearest neighbours can sit side by side: call `add_nnd()` once
with `n = 1` and again with `n = 2`. Because the rank sits between two
underscores and is only digits, the names split unambiguously with
`"^nnd_(\\d+)_(.+)$"`, even when the part after it contains underscores,
as an `across` column named `track_id` gives `nnd_1_track_id`.

## Details

Every identity variable has one of three roles:

- **`across`** — its value must *differ* between a point and its
  neighbour. This is what "another" means: `"individual"` for the
  nearest other animal, `"keypoint"` for the nearest other point on the
  same animal.

- **`within`** — its value must *match*. Added to the temporal context,
  which always applies: points are never compared across timepoints,
  observations, sessions or trials.

- unnamed — unconstrained. Any value may match any other, which is what
  makes the default any-keypoint-to-any-keypoint.

`focal` and `neighbour` then restrict which points are measured *from*
and which are eligible to be measured *to*. Both are named lists of
column to permitted values, and they are independent, so asymmetric
questions like nose-to-tail are expressible.

## See also

[`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
for the vector-level function.

## Examples

``` r
data <- anicore::example_anipoint(
  n_obs = 5,
  n_individuals = 3,
  n_keypoints = 3
)

# Nearest other individual, any keypoint to any keypoint
data |> add_nnd(across = "individual")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.30    0.449       0.687
#>  2          1 head           1     1     2  0.738   1.39        0.605
#>  3          1 head           1     1     3  1.89    0.427       0.944
#>  4          1 head           1     1     4 -0.0974  0.108       0.653
#>  5          1 head           1     1     5 -0.936   0.0223      0.374
#>  6          1 neck           1     1     1 -0.279  -0.855       0.745
#>  7          1 neck           1     1     2 -0.313  -0.287       0.619
#>  8          1 neck           1     1     3  1.07    0.895       0.785
#>  9          1 neck           1     1     4  0.0700  0.0673      0.621
#> 10          1 neck           1     1     5 -0.639  -0.163       0.453
#> # ℹ 35 more rows
#> # ℹ 3 more variables: nnd_1_individual <int>, nnd_1_keypoint <fct>,
#> #   nnd_1_distance <dbl>

# Whose neck is my head nearest to?
data |> add_nnd(
  across = "individual",
  focal = list(keypoint = "head"),
  neighbour = list(keypoint = "neck")
)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.30    0.449       0.687
#>  2          1 head           1     1     2  0.738   1.39        0.605
#>  3          1 head           1     1     3  1.89    0.427       0.944
#>  4          1 head           1     1     4 -0.0974  0.108       0.653
#>  5          1 head           1     1     5 -0.936   0.0223      0.374
#>  6          1 neck           1     1     1 -0.279  -0.855       0.745
#>  7          1 neck           1     1     2 -0.313  -0.287       0.619
#>  8          1 neck           1     1     3  1.07    0.895       0.785
#>  9          1 neck           1     1     4  0.0700  0.0673      0.621
#> 10          1 neck           1     1     5 -0.639  -0.163       0.453
#> # ℹ 35 more rows
#> # ℹ 3 more variables: nnd_1_individual <int>, nnd_1_keypoint <fct>,
#> #   nnd_1_distance <dbl>

# Nearest keypoint within each individual
data |> add_nnd(across = "keypoint", within = "individual")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.30    0.449       0.687
#>  2          1 head           1     1     2  0.738   1.39        0.605
#>  3          1 head           1     1     3  1.89    0.427       0.944
#>  4          1 head           1     1     4 -0.0974  0.108       0.653
#>  5          1 head           1     1     5 -0.936   0.0223      0.374
#>  6          1 neck           1     1     1 -0.279  -0.855       0.745
#>  7          1 neck           1     1     2 -0.313  -0.287       0.619
#>  8          1 neck           1     1     3  1.07    0.895       0.785
#>  9          1 neck           1     1     4  0.0700  0.0673      0.621
#> 10          1 neck           1     1     5 -0.639  -0.163       0.453
#> # ℹ 35 more rows
#> # ℹ 2 more variables: nnd_1_keypoint <fct>, nnd_1_distance <dbl>

# Each keypoint to the same keypoint on the nearest other individual
data |> add_nnd(across = "individual", within = "keypoint")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.30    0.449       0.687
#>  2          1 head           1     1     2  0.738   1.39        0.605
#>  3          1 head           1     1     3  1.89    0.427       0.944
#>  4          1 head           1     1     4 -0.0974  0.108       0.653
#>  5          1 head           1     1     5 -0.936   0.0223      0.374
#>  6          1 neck           1     1     1 -0.279  -0.855       0.745
#>  7          1 neck           1     1     2 -0.313  -0.287       0.619
#>  8          1 neck           1     1     3  1.07    0.895       0.785
#>  9          1 neck           1     1     4  0.0700  0.0673      0.621
#> 10          1 neck           1     1     5 -0.639  -0.163       0.453
#> # ℹ 35 more rows
#> # ℹ 2 more variables: nnd_1_individual <int>, nnd_1_distance <dbl>

# Nearest and second-nearest other individual, side by side
data |>
  add_nnd(across = "individual", n = 1) |>
  add_nnd(across = "individual", n = 2)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.30    0.449       0.687
#>  2          1 head           1     1     2  0.738   1.39        0.605
#>  3          1 head           1     1     3  1.89    0.427       0.944
#>  4          1 head           1     1     4 -0.0974  0.108       0.653
#>  5          1 head           1     1     5 -0.936   0.0223      0.374
#>  6          1 neck           1     1     1 -0.279  -0.855       0.745
#>  7          1 neck           1     1     2 -0.313  -0.287       0.619
#>  8          1 neck           1     1     3  1.07    0.895       0.785
#>  9          1 neck           1     1     4  0.0700  0.0673      0.621
#> 10          1 neck           1     1     5 -0.639  -0.163       0.453
#> # ℹ 35 more rows
#> # ℹ 6 more variables: nnd_1_individual <int>, nnd_1_keypoint <fct>,
#> #   nnd_1_distance <dbl>, nnd_2_individual <int>, nnd_2_keypoint <fct>,
#> #   nnd_2_distance <dbl>
```

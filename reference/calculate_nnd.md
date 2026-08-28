# Calculate distance to the n-th nearest neighbour

Computes, for each point, the distance to the nearest point belonging to
a *different* entity — typically a different individual at the same
moment.

Which columns carry time and position is read from the aniframe's
`variables_when` and `variables_where` metadata. The identity columns
are assigned roles by you, explicitly, because "another animal" and
"another point on this animal" are different questions and the data
cannot tell which one you mean.

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

- data:

  An aniframe.

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

- keypoint_neighbour:

  Deprecated. Use `neighbour = list(keypoint = ...)`.

## Value

The input aniframe with added columns:

- `nnd_distance` — distance to the n-th nearest neighbour

- `nnd_<across>` — which entity that neighbour belongs to

- `nnd_<variable>` — the neighbour's value for each unconstrained
  identity variable (e.g. `nnd_keypoint`)

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
data <- anicore::example_aniframe(
  n_obs = 5,
  n_individuals = 3,
  n_keypoints = 3
)

# Nearest other individual, any keypoint to any keypoint
data |> calculate_nnd(across = "individual")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.705   -0.265      0.690
#>  2          1 head           1     1     2  0.00851 -0.447      0.948
#>  3          1 head           1     1     3  2.03    -1.41       0.919
#>  4          1 head           1     1     4 -1.34    -0.506      0.549
#>  5          1 head           1     1     5  1.16    -0.270      0.903
#>  6          1 neck           1     1     1 -0.614    0.687      0.731
#>  7          1 neck           1     1     2 -1.63     0.705      0.714
#>  8          1 neck           1     1     3 -0.0104   0.991      0.781
#>  9          1 neck           1     1     4 -0.657    1.14       0.757
#> 10          1 neck           1     1     5 -0.670   -1.24       0.765
#> # ℹ 35 more rows
#> # ℹ 3 more variables: nnd_individual <int>, nnd_keypoint <fct>,
#> #   nnd_distance <dbl>

# Whose neck is my head nearest to?
data |> calculate_nnd(
  across = "individual",
  focal = list(keypoint = "head"),
  neighbour = list(keypoint = "neck")
)
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.705   -0.265      0.690
#>  2          1 head           1     1     2  0.00851 -0.447      0.948
#>  3          1 head           1     1     3  2.03    -1.41       0.919
#>  4          1 head           1     1     4 -1.34    -0.506      0.549
#>  5          1 head           1     1     5  1.16    -0.270      0.903
#>  6          1 neck           1     1     1 -0.614    0.687      0.731
#>  7          1 neck           1     1     2 -1.63     0.705      0.714
#>  8          1 neck           1     1     3 -0.0104   0.991      0.781
#>  9          1 neck           1     1     4 -0.657    1.14       0.757
#> 10          1 neck           1     1     5 -0.670   -1.24       0.765
#> # ℹ 35 more rows
#> # ℹ 3 more variables: nnd_individual <int>, nnd_keypoint <fct>,
#> #   nnd_distance <dbl>

# Nearest keypoint within each individual
data |> calculate_nnd(across = "keypoint", within = "individual")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.705   -0.265      0.690
#>  2          1 head           1     1     2  0.00851 -0.447      0.948
#>  3          1 head           1     1     3  2.03    -1.41       0.919
#>  4          1 head           1     1     4 -1.34    -0.506      0.549
#>  5          1 head           1     1     5  1.16    -0.270      0.903
#>  6          1 neck           1     1     1 -0.614    0.687      0.731
#>  7          1 neck           1     1     2 -1.63     0.705      0.714
#>  8          1 neck           1     1     3 -0.0104   0.991      0.781
#>  9          1 neck           1     1     4 -0.657    1.14       0.757
#> 10          1 neck           1     1     5 -0.670   -1.24       0.765
#> # ℹ 35 more rows
#> # ℹ 2 more variables: nnd_keypoint <fct>, nnd_distance <dbl>

# Each keypoint to the same keypoint on the nearest other individual
data |> calculate_nnd(across = "individual", within = "keypoint")
#> # Individuals: 1, 2, 3
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -0.705   -0.265      0.690
#>  2          1 head           1     1     2  0.00851 -0.447      0.948
#>  3          1 head           1     1     3  2.03    -1.41       0.919
#>  4          1 head           1     1     4 -1.34    -0.506      0.549
#>  5          1 head           1     1     5  1.16    -0.270      0.903
#>  6          1 neck           1     1     1 -0.614    0.687      0.731
#>  7          1 neck           1     1     2 -1.63     0.705      0.714
#>  8          1 neck           1     1     3 -0.0104   0.991      0.781
#>  9          1 neck           1     1     4 -0.657    1.14       0.757
#> 10          1 neck           1     1     5 -0.670   -1.24       0.765
#> # ℹ 35 more rows
#> # ℹ 2 more variables: nnd_individual <int>, nnd_distance <dbl>
```

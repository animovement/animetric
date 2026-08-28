# Compute the centroid of an identity level

The mean position of the members of one identity variable, at each
position of every other. Which variable is collapsed is the caller's
choice: the keypoints of an animal, the animals of a team, or any other
level the frame declares.

## Usage

``` r
compute_centroid(
  data,
  across = NULL,
  include = NULL,
  exclude = NULL,
  name = "centroid"
)
```

## Arguments

- data:

  An aniframe with Cartesian coordinates.

- across:

  Identity variables to collapse — the dimensions the summary ranges
  over. Defaults to the finest one the frame declares, which is the
  keypoint-style summary. Collapsing every level gives a single point
  per position.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed.

- name:

  Name for the summary member. Default is `"centroid"`.

## Value

An aniframe containing only the summary member. Coordinate values are
the mean of the members included, with `NA`s removed. Confidence is
`NA`. Missing coordinate dimensions return `NA`.

## See also

[`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md),
which appends the result to the frame.

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 20, n_individuals = 2, n_keypoints = 3)

# The centroid of each animal's keypoints
compute_centroid(af, across = "keypoint")
#> # Individuals: 1, 2
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 centroid       1     1     1  0.364   0.506         NA
#>  2          1 centroid       1     1     2  1.29    1.23          NA
#>  3          1 centroid       1     1     3 -0.0271  0.885         NA
#>  4          1 centroid       1     1     4 -0.183  -0.748         NA
#>  5          1 centroid       1     1     5 -1.10   -0.472         NA
#>  6          1 centroid       1     1     6 -0.259  -0.845         NA
#>  7          1 centroid       1     1     7  0.149  -0.322         NA
#>  8          1 centroid       1     1     8  0.163  -0.325         NA
#>  9          1 centroid       1     1     9 -1.10   -0.678         NA
#> 10          1 centroid       1     1    10 -0.199  -0.249         NA
#> # ℹ 30 more rows

# From a subset of them
compute_centroid(af, across = "keypoint", include = c("head", "neck"))
#> # Individuals: 1, 2
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x      y confidence
#>         <int> <fct>      <int> <int> <int>   <dbl>  <dbl>      <dbl>
#>  1          1 centroid       1     1     1 -0.0431  0.773         NA
#>  2          1 centroid       1     1     2  1.71    0.732         NA
#>  3          1 centroid       1     1     3 -1.18    1.25          NA
#>  4          1 centroid       1     1     4 -0.343  -0.688         NA
#>  5          1 centroid       1     1     5 -0.652  -0.121         NA
#>  6          1 centroid       1     1     6 -0.179  -0.388         NA
#>  7          1 centroid       1     1     7  0.413  -0.512         NA
#>  8          1 centroid       1     1     8 -0.366  -1.07          NA
#>  9          1 centroid       1     1     9 -0.885  -1.19          NA
#> 10          1 centroid       1     1    10 -0.144   0.153         NA
#> # ℹ 30 more rows

# The centroid of the animals themselves, one per keypoint
compute_centroid(af, across = "individual")
#> # Individuals: centroid
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time          x       y confidence
#>    <fct>      <fct>      <int> <int> <int>      <dbl>   <dbl>      <dbl>
#>  1 centroid   head           1     1     1 -0.861      0.932          NA
#>  2 centroid   head           1     1     2  0.677      0.713          NA
#>  3 centroid   head           1     1     3 -0.463      0.449          NA
#>  4 centroid   head           1     1     4 -0.634     -0.734          NA
#>  5 centroid   head           1     1     5 -0.297      0.589          NA
#>  6 centroid   head           1     1     6  0.0000156 -0.486          NA
#>  7 centroid   head           1     1     7  0.491      0.757          NA
#>  8 centroid   head           1     1     8 -0.214      0.731          NA
#>  9 centroid   head           1     1     9 -0.874     -0.138          NA
#> 10 centroid   head           1     1    10 -1.27       0.0126         NA
#> # ℹ 50 more rows

# One point for the whole group, per position
compute_centroid(af, across = c("individual", "keypoint"))
#> # Individuals: centroid
#> # Keypoints:   centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time       x       y confidence
#>    <fct>      <fct>      <int> <int> <int>   <dbl>   <dbl>      <dbl>
#>  1 centroid   centroid       1     1     1 -0.0444  0.690          NA
#>  2 centroid   centroid       1     1     2  0.678   0.765          NA
#>  3 centroid   centroid       1     1     3 -0.301   0.110          NA
#>  4 centroid   centroid       1     1     4 -0.375  -0.128          NA
#>  5 centroid   centroid       1     1     5 -0.652  -0.207          NA
#>  6 centroid   centroid       1     1     6 -0.132  -0.245          NA
#>  7 centroid   centroid       1     1     7  0.133   0.126          NA
#>  8 centroid   centroid       1     1     8  0.203  -0.0469         NA
#>  9 centroid   centroid       1     1     9 -0.821  -0.0460         NA
#> 10 centroid   centroid       1     1    10 -0.197  -0.0814         NA
#> 11 centroid   centroid       1     1    11 -0.124   0.532          NA
#> 12 centroid   centroid       1     1    12 -0.595  -0.0592         NA
#> 13 centroid   centroid       1     1    13 -0.113   0.258          NA
#> 14 centroid   centroid       1     1    14 -0.623   0.308          NA
#> 15 centroid   centroid       1     1    15  0.196  -0.0688         NA
#> 16 centroid   centroid       1     1    16  0.489  -0.331          NA
#> 17 centroid   centroid       1     1    17  0.0316 -0.108          NA
#> 18 centroid   centroid       1     1    18  0.0577  0.206          NA
#> 19 centroid   centroid       1     1    19  0.294   0.0970         NA
#> 20 centroid   centroid       1     1    20 -0.357  -0.349          NA
```

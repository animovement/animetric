# Add a centroid to an aniframe

Appends the centroid of one identity level to the frame, as a new member
of that level. The rest of the data is returned untouched.

Which levels are collapsed is the caller's choice. On pose data for a
team, the default gives each player their own centre;
`across = "individual"` gives one centre per keypoint across the
players; and collapsing both gives the single point the whole team
occupies.

A level that did not actually vary keeps its value rather than taking
the summary's name — an individual's strain is still its strain, since
nothing was averaged over it.

## Usage

``` r
add_centroid(
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
  over. Defaults to the finest one the frame declares. Collapsing every
  level gives a single point per position.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed.

- name:

  Name for the new member. Default is `"centroid"`.

## Value

The aniframe, with the centroid appended as extra rows. The collapsed
identity column comes back as a factor, since it now holds a named
member that an integer column could not.

## See also

[`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md),
which returns the centroid on its own.

## Examples

``` r
af <- anicore::example_aniframe(n_obs = 20, n_individuals = 2, n_keypoints = 3)

# Each animal gains a centroid keypoint
add_centroid(af, across = "keypoint")
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.40    -0.504      0.186
#>  2          1 head           1     1     2  0.255   -1.19       0.615
#>  3          1 head           1     1     3 -2.44    -0.752      0.620
#>  4          1 head           1     1     4 -0.00557  1.46       0.692
#>  5          1 head           1     1     5  0.622   -0.829      0.710
#>  6          1 head           1     1     6  1.15     0.290      0.577
#>  7          1 head           1     1     7 -1.82    -0.480      0.764
#>  8          1 head           1     1     8 -0.247   -0.605      0.809
#>  9          1 head           1     1     9 -0.244    1.46       0.674
#> 10          1 head           1     1    10 -0.283    0.150      0.893
#> # ℹ 150 more rows

# From a subset of the keypoints
add_centroid(af, across = "keypoint", include = c("head", "neck"))
#> # Individuals: 1, 2
#> # Keypoints:   head, neck, shoulder_right, centroid
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>         <int> <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1          1 head           1     1     1 -1.40    -0.504      0.186
#>  2          1 head           1     1     2  0.255   -1.19       0.615
#>  3          1 head           1     1     3 -2.44    -0.752      0.620
#>  4          1 head           1     1     4 -0.00557  1.46       0.692
#>  5          1 head           1     1     5  0.622   -0.829      0.710
#>  6          1 head           1     1     6  1.15     0.290      0.577
#>  7          1 head           1     1     7 -1.82    -0.480      0.764
#>  8          1 head           1     1     8 -0.247   -0.605      0.809
#>  9          1 head           1     1     9 -0.244    1.46       0.674
#> 10          1 head           1     1    10 -0.283    0.150      0.893
#> # ℹ 150 more rows

# One centre per keypoint, across the animals
add_centroid(af, across = "individual", name = "group")
#> # Individuals: 1, 2, group
#> # Keypoints:   head, neck, shoulder_right
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>    <fct>      <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1 1          head           1     1     1 -1.40    -0.504      0.186
#>  2 1          head           1     1     2  0.255   -1.19       0.615
#>  3 1          head           1     1     3 -2.44    -0.752      0.620
#>  4 1          head           1     1     4 -0.00557  1.46       0.692
#>  5 1          head           1     1     5  0.622   -0.829      0.710
#>  6 1          head           1     1     6  1.15     0.290      0.577
#>  7 1          head           1     1     7 -1.82    -0.480      0.764
#>  8 1          head           1     1     8 -0.247   -0.605      0.809
#>  9 1          head           1     1     9 -0.244    1.46       0.674
#> 10 1          head           1     1    10 -0.283    0.150      0.893
#> # ℹ 170 more rows

# The single point the whole group occupies
add_centroid(af, across = c("individual", "keypoint"), name = "group")
#> # Individuals: 1, 2, group
#> # Keypoints:   head, neck, shoulder_right, group
#> # Sessions:    1
#> # Trials:      1
#>    individual keypoint session trial  time        x      y confidence
#>    <fct>      <fct>      <int> <int> <int>    <dbl>  <dbl>      <dbl>
#>  1 1          head           1     1     1 -1.40    -0.504      0.186
#>  2 1          head           1     1     2  0.255   -1.19       0.615
#>  3 1          head           1     1     3 -2.44    -0.752      0.620
#>  4 1          head           1     1     4 -0.00557  1.46       0.692
#>  5 1          head           1     1     5  0.622   -0.829      0.710
#>  6 1          head           1     1     6  1.15     0.290      0.577
#>  7 1          head           1     1     7 -1.82    -0.480      0.764
#>  8 1          head           1     1     8 -0.247   -0.605      0.809
#>  9 1          head           1     1     9 -0.244    1.46       0.674
#> 10 1          head           1     1    10 -0.283    0.150      0.893
#> # ℹ 130 more rows
```

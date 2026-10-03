# Compute the centroid of an identity level

**Deprecated.** Use
[`compute_point()`](https://animovement.dev/animetric/reference/compute_point.md),
whose default `method = "centroid"` does the same.

This returns exactly what it did: the centroid, with any declared
orientation left `NA`.

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

  An anipoint with Cartesian coordinates.

- across:

  Identity variables to collapse — the dimensions the new point is
  derived over. Required when the frame declares more than one identity
  variable, since their order is not a hierarchy and there is no finest
  one to assume; with a single identity variable, that one is the
  default. Collapsing every level gives a single point per position.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed. A midpoint is the centroid of two
  members: `include = c("ear_l", "ear_r")`.

- name:

  Name for the summary member. Default is `"centroid"`.

## Value

An anipoint containing only the centroid.

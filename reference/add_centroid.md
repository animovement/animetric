# Add a centroid to an anipoint

**\[deprecated\]**

Use
[`add_point()`](https://animovement.dev/animetric/reference/add_point.md),
whose default `method = "centroid"` does the same, and which can also
derive a median, a confidence-weighted centroid or a point by a rule of
your own, and derives a declared orientation for the new member.

This returns exactly what it did: the centroid as a new member, with any
declared orientation left `NA` for it.

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

  Name for the new member. Default is `"centroid"`.

## Value

The anipoint, with the centroid appended as extra rows.

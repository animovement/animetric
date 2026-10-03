# Append a derived point to the frame it came from

Append a derived point to the frame it came from

## Usage

``` r
append_point(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL,
  orientation = TRUE,
  call = rlang::caller_env()
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

- method:

  How each coordinate is derived from the members' values:

  `"centroid"`

  :   The mean.

  `"median"`

  :   The median, per axis: robust to a single stray point.

  `"weighted"`

  :   The mean weighted by `confidence`, so poorly tracked points count
      for less. Needs a `confidence` column.

  a function

  :   Applied to each axis's values, with missing values removed,
      returning one number; e.g. `\(x) mean(x, trim = 0.1)`.

  Missing values are ignored; a moment where every member is missing
  gives `NA`.

- include, exclude:

  Values of the collapsed level to keep or leave out. Only meaningful
  when one level is collapsed. A midpoint is the centroid of two
  members: `include = c("ear_l", "ear_r")`.

- name:

  Name for the new member. Defaults to `"centroid"`, `"median"` or
  `"weighted_centroid"` after `method`; required when `method` is a
  function.

- orientation:

  Whether to derive a declared orientation for the new member, or leave
  it `NA` as
  [`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md)
  did.

- call:

  The calling environment, for error messages.

## Value

The anipoint with the new member appended.

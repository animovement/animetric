# The identity variables a summary collapses

Which levels are summarised is the caller's to choose, and there is no
guessing it: `variables_what` is documented as coarse to fine, but
nothing enforces that and orthogonal attributes do not nest at all
(animovement/anicore#140, animovement/anicore#141). A frame declaring
more than one identity variable has to be told.

## Usage

``` r
resolve_collapsed_identity(data, across = NULL, call = rlang::caller_env())
```

## Arguments

- data:

  An aniframe.

- across:

  Identity variables to collapse, or `NULL` for the finest one.

## Value

Character vector naming the columns to collapse.

## Details

A frame declaring exactly one has nothing to be ambiguous about.

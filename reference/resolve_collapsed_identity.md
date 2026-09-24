# The identity variables a summary collapses

Which levels are summarised is the caller's to choose, and there is no
guessing it: the order of the identity keys is what detection emits
rather than a hierarchy a frame asserts, and identity variables need not
nest at all (animovement/anicore#141). A frame declaring more than one
identity variable has to be told.

## Usage

``` r
resolve_collapsed_identity(data, across = NULL, call = rlang::caller_env())
```

## Arguments

- data:

  An anipoint.

- across:

  Identity variables to collapse, or `NULL` for the finest one.

## Value

Character vector naming the columns to collapse.

## Details

A frame declaring exactly one has nothing to be ambiguous about.

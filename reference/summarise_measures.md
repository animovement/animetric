# Summarise measures with linear or circular statistics

Summarise measures with linear or circular statistics

## Usage

``` r
summarise_measures(
  data,
  cols,
  measures,
  linear,
  circular = character(),
  hint = NULL,
  call = rlang::caller_env()
)
```

## Arguments

- data:

  An aniframe.

- cols:

  Columns to summarise, or `NULL` for the defaults.

- measures:

  `"median_mad"` or `"mean_sd"`.

- linear:

  The class's default linear measures.

- circular:

  The angles the class knows of, as a named character vector: output
  stem to column.

- hint:

  What to suggest when there is nothing to summarise.

- call:

  The calling environment, for error messages.

## Value

A data frame with one row per group.

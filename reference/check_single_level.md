# Check that include and exclude name one collapsed level

Check that include and exclude name one collapsed level

## Usage

``` r
check_single_level(identity_cols, include, exclude, call = rlang::caller_env())
```

## Arguments

- identity_cols:

  The identity variables being collapsed.

- include, exclude:

  As for
  [`add_point()`](https://animovement.dev/animetric/reference/add_point.md).

- call:

  The calling environment, for error messages.

## Value

`TRUE`, invisibly.

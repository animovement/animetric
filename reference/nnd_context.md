# Columns that must match between a point and its neighbours

Every temporal variable the frame declares, plus whatever the caller
adds. The temporal part is not optional: comparing positions across
timepoints, or across observations that each start at their own time
origin, is never what is wanted.

## Usage

``` r
nnd_context(data, variables, within)
```

## Arguments

- data:

  An aniframe.

- variables:

  Output of
  [`nnd_variables()`](https://animovement.dev/animetric/reference/nnd_variables.md).

- within:

  Extra columns supplied by the caller.

## Value

Character vector of column names.

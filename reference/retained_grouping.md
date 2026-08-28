# The columns a summary should keep, having collapsed the finest identity

Everything the frame groups by except the level being summarised over,
plus the index – one row per remaining entity per position.

## Usage

``` r
retained_grouping(data, collapsed)
```

## Arguments

- data:

  An aniframe.

- collapsed:

  The identity column being summarised across.

## Value

Character vector of column names.

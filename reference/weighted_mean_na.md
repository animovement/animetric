# A weighted mean that ignores missing values and weights

A weighted mean that ignores missing values and weights

## Usage

``` r
weighted_mean_na(v, w)
```

## Arguments

- v:

  Numeric vector of values.

- w:

  Numeric vector of weights.

## Value

The weighted mean of the values whose value and weight are both present,
or `NA` when they carry no weight.

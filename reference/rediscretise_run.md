# Rediscretise one unbroken stretch of path

Rediscretise one unbroken stretch of path

## Usage

``` r
rediscretise_run(p, time, step_length)
```

## Arguments

- p:

  A numeric matrix of positions, no missing values, at least two rows.

- time:

  The index.

- step_length:

  The step length, positive.

## Value

A list of `position` and `time` of the new points, starting with the
first recorded one.

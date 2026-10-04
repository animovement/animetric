# Rediscretise a path to a constant step length

Walks along the path, as straight lines between the recorded positions,
and places a point where it first leaves a circle of radius
`step_length` around the last point placed (Bovet & Benhamou 1988).
Missing positions break the path: each unbroken stretch is rediscretised
on its own.

## Usage

``` r
rediscretise_path(position, time, step_length)
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- time:

  The index, for the time at which the path reaches each new point.

- step_length:

  The step length, in the unit of the positions.

## Value

A list of `position` (a matrix, one row per new point), `time`, and
`run` (which unbroken stretch each point belongs to).

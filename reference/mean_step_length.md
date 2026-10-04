# The step length to rediscretise a trajectory at

The mean step between rows, weighted by its length: the average step
over the distance travelled rather than over time. Time spent still adds
many short steps, which would shorten a plain mean however long the
animal paused, but adds almost nothing to this one.

## Usage

``` r
mean_step_length(position)
```

## Arguments

- position:

  A data frame of positions, one column per axis.

## Value

A number, ignoring steps to or from a missing position. `NaN` when the
trajectory never moves.

# Cumulative turning of a sequence of velocity vectors

Starts at 0 and accumulates the angle between consecutive velocities
where the animal is moving, so turning across a pause is counted once,
when it moves off again. Stationary or missing rows carry the running
total.

## Usage

``` r
cumulative_turning(v, moving)
```

## Arguments

- v:

  Numeric matrix of velocities, one row per observation.

- moving:

  Logical vector, whether each row's velocity is defined and non-zero.

## Value

Numeric vector, in radians.

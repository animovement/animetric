# Is a step long enough to give a direction?

Is a step long enough to give a direction?

## Usage

``` r
has_direction(step, min_step)
```

## Arguments

- step:

  Distance moved per sample.

- min_step:

  The shortest step that gives a direction.

## Value

Logical vector: `FALSE` where `step` is missing, zero, or shorter than
`min_step`.

# Circular quantiles (Fisher & Lee 1995)

Returns the angle θ such that a proportion *p* of the data lie clockwise
from θ. The algorithm works directly on the circle, so it is immune to
the artificial break at 0/2π that affects simple rank‑based methods.
Consequently the 0.5‑quantile coincides with the circular median.

## Usage

``` r
circ_quantile(x, probs = c(0.25, 0.5, 0.75), na_rm = TRUE)
```

## Arguments

- x:

  Numeric vector of angles (radians)

- probs:

  Numeric vector of probabilities in \[0,1\].

- na_rm:

  Logical; if TRUE, remove NA values before computation

## Value

Vector of quantile angles (radians) wrapped to \[0, 2π).

## References

Fisher, N. I. & Lee, A. J. (1995). *Statistical Analysis of Circular
Data*. Cambridge University Press. Chapter 3.

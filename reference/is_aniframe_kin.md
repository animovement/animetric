# Check if object is an aniframe_kin

Check if object is an aniframe_kin

## Usage

``` r
is_aniframe_kin(x)
```

## Arguments

- x:

  An object to test

## Value

Logical: TRUE if x inherits from aniframe_kin

## Examples

``` r
kin <- calculate_kinematics(
  anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
is_aniframe_kin(kin)
#> [1] TRUE

# An anipoint without kinematics is not one
is_aniframe_kin(anicore::example_anipoint(n_obs = 3))
#> [1] FALSE
```

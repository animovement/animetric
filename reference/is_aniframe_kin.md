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

Logical: TRUE if x inherits from aniframe

## Examples

``` r
kin <- calculate_kinematics(
  aniframe::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 1)
)
is_aniframe_kin(kin)
#> [1] TRUE

# An aniframe without kinematics is not one
is_aniframe_kin(aniframe::example_aniframe(n_obs = 3))
#> [1] FALSE
```

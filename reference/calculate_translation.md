# Calculate translational kinematics

Works on any number of Cartesian axes. Velocity and acceleration
components are named by axis role (`v_x`, `a_x`, ...), speed and step
length are Euclidean norms over the axes.

## Usage

``` r
calculate_translation(data)
```

## Arguments

- data:

  A Cartesian anipoint.

## Value

The anipoint with added translational kinematic columns

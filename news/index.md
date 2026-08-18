# Changelog

## animetric 0.4.0 (2026-08-18)

### Breaking changes

- Removed the `aniframe_kin2d` and `aniframe_kin3d` classes.
  [`calculate_kinematics()`](http://animovement.dev/animetric/reference/calculate_kinematics.md)
  set them, but nothing ever read them — no predicate, no method, no
  test in any package — and the dimensionality they encoded already
  lives in the `coordinate_system` metadata field. Kinematics output
  still carries `aniframe_kin`, which is what the `summarise_*()`
  functions dispatch on.

## animetric 0.3.0

This release brings a re-factoring of the `calculate` and `summarise`
functions, along with new `tortuosity` metrics and a bucnh of tests. I’m
also excited to bring on the first social/collective metric: “Nearest
Neighbour Distance” (NND). The calculations are made thoughout the time
series with
[`calculate_nnd()`](http://animovement.dev/animetric/reference/calculate_nnd.md).

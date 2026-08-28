# animetric 0.4.0 (2026-08-18)

## Changed

* `wrap_angle()` and `unwrap_angle()` are re-exported from `anicore` rather than `anispace`, following their move (animovement/aniframe#128). The re-exports themselves are unchanged.

## Breaking changes

* Removed the `aniframe_kin2d` and `aniframe_kin3d` classes. `calculate_kinematics()` set them, but nothing ever read them — no predicate, no method, no test in any package — and the dimensionality they encoded already lives in the `coordinate_system` metadata field. Kinematics output still carries `aniframe_kin`, which is what the `summarise_*()` functions dispatch on.

## Bug fixes

* `calculate_nnd()` checks that the `individual` and `keypoint` columns exist before reading them. `data$col` on a frame without the column returns `NULL` with a warning, and `all(is.na(NULL))` is `TRUE`, so an absent column was reported as an all-`NA` one — and every call warned on the way. Surfaced by aniframe 0.7.0, which no longer adds a `keypoint` column beside an existing identity.

## Internal

* Documentation regenerated with roxygen2 8.1.0, matching the rest of the ecosystem.

# animetric 0.3.0

This release brings a re-factoring of the `calculate` and `summarise` functions, along with new `tortuosity` metrics and a bucnh of tests. 
I'm also excited to bring on the first social/collective metric: "Nearest Neighbour Distance" (NND). The calculations are made thoughout the time series with `calculate_nnd()`.
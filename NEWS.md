# animetric (development version)

## Changed

* The core data structures come from `anicore`, which is what the `aniframe` package was renamed to in its 0.8.0 (animovement/anicore#84). The `aniframe` class keeps its name; only the package providing it changed.

## Removed

* The re-exports of `as_aniframe()`, `is_aniframe()`, `ensure_is_aniframe()`, `deg_to_rad()`, `rad_to_deg()`, `wrap_angle()`, `unwrap_angle()`, `calculate_angular_difference()` and `diff_angle()`. **Calls to these through `animetric::` need repointing at `anicore::` or `anispace::`.** animetric still uses them internally — it just has no reason to publish another package's interface as its own, which left the same function documented in two places and animetric's exports growing whenever anicore's did.

# animetric 0.4.0 (2026-08-18)

## Changed

* Removed the `aniframe_kin2d` and `aniframe_kin3d` classes. `calculate_kinematics()` set them, but nothing ever read them — no predicate, no method, no test in any package — and the dimensionality they encoded already lives in the `coordinate_system` metadata field. Kinematics output still carries `aniframe_kin`, which is what the `summarise_*()` functions dispatch on.

## Fixed

* `calculate_nnd()` checks that the `individual` and `keypoint` columns exist before reading them. An absent column was reported as an all-`NA` one, and every call warned on the way. Surfaced by aniframe 0.7.0, which no longer adds a `keypoint` column beside an existing identity.

# animetric 0.3.2

## Changed

* Requires aniframe 0.4.1.

# animetric 0.3.1

## Added

* `summarise_aniframe()` and `summarise_tortuosity()`, alongside `summarize_*()` spellings for `summarise_aniframe()`, `summarise_kinematics()` and `summarise_tortuosity()`.

## Fixed

* `calculate_tortuosity()` preserves the classes of the frame it was given.
* `calculate_nnd()` keeps the incoming classes rather than returning a plain frame.

# animetric 0.3.0 (2025-12-04)

The `calculate_*()` and `summarise_*()` families are reworked, tortuosity metrics arrive, and with `calculate_nnd()` the package gains its first social metric.

## Added

* `calculate_nnd()` and `compute_nnd()` compute nearest-neighbour distance through a time series — the first collective metric in the package.

## Removed

* `calculate_kinematics_2d()`, `calculate_kinematics_3d()`, `calculate_tortuosity_2d()` and `calculate_tortuosity_3d()`. `calculate_kinematics()` and `calculate_tortuosity()` branch on dimensionality themselves, from the frame's `coordinate_system`.

# animetric 0.2.1

## Changed

* Spatial transformations are taken from anispace, following their move out of aniframe.

# animetric 0.2.0

The package takes its present shape: kinematics, path complexity, angles and summaries.

## Added

* Kinematics: `calculate_kinematics()`, with `calculate_kinematics_2d()` and `_3d()` behind it, and `differentiate()`.
* Path complexity: `calculate_tortuosity()` with `_2d()` and `_3d()` variants, `compute_sinuosity()`, `compute_straightness()` and `compute_emax()`.
* Spatial summaries: `compute_centroid()`.
* Circular statistics: `mean_angle()` and `median_angle()`.
* Summaries: `summarise_kinematics()` and `summarise_keypoints()`.
* `is_aniframe_kin()` to test whether a frame carries kinematics.
* Angle helpers re-exported from aniframe: `deg_to_rad()`, `rad_to_deg()`, `wrap_angle()`, `unwrap_angle()`, `diff_angle()` and `calculate_angular_difference()`.

# animetric 0.1.0

First commit. animetric computes movement metrics from an aniframe — kinematics, path complexity and summaries over a trajectory.

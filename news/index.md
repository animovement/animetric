# Changelog

## animetric 0.4.0 (2026-08-18)

### Changed

- Removed the `aniframe_kin2d` and `aniframe_kin3d` classes.
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  set them, but nothing ever read them — no predicate, no method, no
  test in any package — and the dimensionality they encoded already
  lives in the `coordinate_system` metadata field. Kinematics output
  still carries `aniframe_kin`, which is what the `summarise_*()`
  functions dispatch on.

### Fixed

- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  checks that the `individual` and `keypoint` columns exist before
  reading them. An absent column was reported as an all-`NA` one, and
  every call warned on the way. Surfaced by aniframe 0.7.0, which no
  longer adds a `keypoint` column beside an existing identity.

## animetric 0.3.2

### Changed

- Requires aniframe 0.4.1.

## animetric 0.3.1

### Added

- [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md),
  alongside `summarize_*()` spellings for
  [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md),
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md).

### Fixed

- [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  preserves the classes of the frame it was given.
- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  keeps the incoming classes rather than returning a plain frame.

## animetric 0.3.0 (2025-12-04)

The `calculate_*()` and `summarise_*()` families are reworked,
tortuosity metrics arrive, and with
[`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
the package gains its first social metric.

### Added

- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  and
  [`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
  compute nearest-neighbour distance through a time series — the first
  collective metric in the package.

### Removed

- `calculate_kinematics_2d()`, `calculate_kinematics_3d()`,
  `calculate_tortuosity_2d()` and `calculate_tortuosity_3d()`.
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  and
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  branch on dimensionality themselves, from the frame’s
  `coordinate_system`.

## animetric 0.2.1

### Changed

- Spatial transformations are taken from anispace, following their move
  out of aniframe.

## animetric 0.2.0

The package takes its present shape: kinematics, path complexity, angles
and summaries.

### Added

- Kinematics:
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  with `calculate_kinematics_2d()` and `_3d()` behind it, and
  [`differentiate()`](https://animovement.dev/animetric/reference/differentiate.md).
- Path complexity:
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  with `_2d()` and `_3d()` variants,
  [`compute_sinuosity()`](https://animovement.dev/animetric/reference/compute_sinuosity.md),
  [`compute_straightness()`](https://animovement.dev/animetric/reference/compute_straightness.md)
  and
  [`compute_emax()`](https://animovement.dev/animetric/reference/compute_emax.md).
- Spatial summaries:
  [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md).
- Circular statistics:
  [`mean_angle()`](https://animovement.dev/animetric/reference/mean_angle.md)
  and
  [`median_angle()`](https://animovement.dev/animetric/reference/median_angle.md).
- Summaries:
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and
  [`summarise_keypoints()`](https://animovement.dev/animetric/reference/summarise_keypoints.md).
- [`is_aniframe_kin()`](https://animovement.dev/animetric/reference/is_aniframe_kin.md)
  to test whether a frame carries kinematics.
- Angle helpers re-exported from aniframe:
  [`deg_to_rad()`](https://animovement.dev/aniframe/reference/deg_to_rad.html),
  [`rad_to_deg()`](https://animovement.dev/aniframe/reference/rad_to_deg.html),
  [`wrap_angle()`](https://animovement.dev/anispace/reference/wrap_angle.html),
  [`unwrap_angle()`](https://animovement.dev/anispace/reference/unwrap_angle.html),
  [`diff_angle()`](https://animovement.dev/anispace/reference/diff_angle.html)
  and
  [`calculate_angular_difference()`](https://animovement.dev/anispace/reference/calculate_angular_difference.html).

## animetric 0.1.0

First commit. animetric computes movement metrics from an aniframe —
kinematics, path complexity and summaries over a trajectory.

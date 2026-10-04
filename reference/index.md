# Package index

## Add

These return your aniframe with something added: columns of per-row
measures, a declared orientation, or a derived point.

- [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
  : Add kinematic measures to trajectory data
- [`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)
  : Add tortuosity measures over sliding windows
- [`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md)
  **\[experimental\]** : Declare orientation from the positions of
  points
- [`add_point()`](https://animovement.dev/animetric/reference/add_point.md)
  : Add a derived point to an anipoint

## Calculate

These functions take your aniframe as input and give you results in
return.

- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  : Calculate distance to the n-th nearest neighbour

## Summarise

One row per group: the distribution of per-row measures with
[`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md),
or the geometry of each whole trajectory with
[`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md).

- [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  [`summarize_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  **\[experimental\]** : Summarise the time series of an aniframe
- [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
  [`summarize_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
  : Summarise each trajectory as a whole

## Compute

These functions are cogs used in the `add_`, `calculate_` and
`summarise_` functions.

- [`compute_gradient()`](https://animovement.dev/animetric/reference/compute_gradient.md)
  : Compute numerical derivatives on possibly uneven grids
- [`differentiate()`](https://animovement.dev/animetric/reference/differentiate.md)
  : Differentiate a numeric series (optionally repeatedly)
- [`compute_straightness()`](https://animovement.dev/animetric/reference/compute_straightness.md)
  : Compute straightness index from precomputed vectors
- [`compute_sinuosity()`](https://animovement.dev/animetric/reference/compute_sinuosity.md)
  : Compute sinuosity index from precomputed vectors
- [`compute_emax()`](https://animovement.dev/animetric/reference/compute_emax.md)
  : Compute E_max (maximum expected displacement) from pre‑computed
  vectors
- [`compute_point()`](https://animovement.dev/animetric/reference/compute_point.md)
  : Compute a derived point of an identity level
- [`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
  : Compute nearest neighbour distances within one group

## Deprecated

Kept for one release; see each page for its replacement.

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  **\[deprecated\]** : Calculate kinematic measures from trajectory data
- [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  **\[deprecated\]** : Calculate tortuosity metrics over sliding windows
- [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  [`summarize_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  **\[deprecated\]** : Calculate kinematic summary statistics
- [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  [`summarize_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  **\[deprecated\]** : Calculate tortuosity summary statistics
- [`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md)
  **\[deprecated\]** : Add a centroid to an anipoint
- [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md)
  **\[deprecated\]** : Compute the centroid of an identity level
- [`is_aniframe_kin()`](https://animovement.dev/animetric/reference/is_aniframe_kin.md)
  **\[deprecated\]** : Test whether a frame holds kinematics

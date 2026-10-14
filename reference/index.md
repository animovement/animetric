# Package index

## Add

These return your aniframe with something added: columns of per-row
measures, a declared orientation, or a derived point.

- [`add_kinematics()`](https://animovement.dev/animetric/reference/add_kinematics.md)
  : Add kinematic measures to trajectory data
- [`add_tortuosity()`](https://animovement.dev/animetric/reference/add_tortuosity.md)
  : Add tortuosity measures over sliding windows
- [`add_nnd()`](https://animovement.dev/animetric/reference/add_nnd.md)
  : Add the distance to the n-th nearest neighbour
- [`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md)
  **\[experimental\]** : Declare orientation from the positions of
  points
- [`add_point()`](https://animovement.dev/animetric/reference/add_point.md)
  : Add a derived point to an anipoint

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

These functions are cogs used in the `add_` and `summarise_` functions.

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

- [`compute_min_step()`](https://animovement.dev/animetric/reference/compute_min_step.md)
  **\[experimental\]** :

  The minimum step `"auto"` chooses for each trajectory

- [`compute_point()`](https://animovement.dev/animetric/reference/compute_point.md)
  : Compute a derived point of an identity level

- [`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
  : Compute nearest neighbour distances within one group

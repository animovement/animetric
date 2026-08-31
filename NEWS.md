# animetric (development version)

## Removed

* `mean_angle()` and `median_angle()` are removed. Use `anicore::circ_mean()` and `anicore::circ_median()`, which are attached by `library(animovement)` (animovement/anicore#147). The circular statistics live in one place now, and these were the two that had drifted: `mean_angle()` duplicated `circ_mean()` exactly, and `median_angle()` was not a circular median at all.

  `median_angle()` took the median of the sine and cosine components, which is not rotation-equivariant — rotating every angle in a sample by the same amount moved its answer by a different amount, so the result depended on where the circle was cut. `anicore::circ_median()` is Fisher's circular median and does not have that defect, so it is a replacement that returns **different numbers**. Any stored values computed with `median_angle()` were frame-dependent.

## Changed

* The circular summaries in `summarise_kinematics()` come from anicore, and the `circular` package is no longer needed at all (animovement/anicore#147). It was a soft dependency behind a `check_installed()` prompt, so the first call to `summarise_kinematics()` on a fresh install used to stop and ask to install a package — for two columns of the summary table.

* `mean_heading` is reported in `[0, 2*pi)`, like `median_heading` already was. It previously came back in `(-pi, pi]`, so the two summaries of the same column disagreed about where the circle starts; near `+/-pi` that showed up as a mean of `-3.13` beside a median of `3.15`. Both now use the range `anicore::wrap_angle()` gives by default. The direction is unchanged — only how it is written down.

## Fixed

* **`median_heading` could be 180 degrees wrong**, and is now correct (animovement/anicore#147). Where two directions tie for the circular median, the old implementation averaged them arithmetically — it read the tied pair out of an undocumented attribute of `circular::median.circular()`'s return value and called `mean()` on it. When the tie straddles zero, the arithmetic mean of the two is their antipode. Headings tied at 0.1 and 5.8 radians gave 2.95 radians, or 169 degrees, where the answer is 349 degrees:

  ```r
  # the two tied directions, and what each way of averaging them gives
  #   arithmetic: (0.1 + 5.8) / 2 = 2.95   -> 169 degrees, the antipode
  #   circular:   anicore::circ_median()   -> 349 degrees
  ```

  Nothing signalled it: no `NA`, no warning, and a plausible direction. Heading distributions straddle zero routinely, so this was not a corner case. `anicore::circ_median()` averages tied directions on the circle, and a grid search over the definition — the direction minimising the summed angular distance — confirms which of the two is the median. Any stored `median_heading` may need recomputing.

* `sd_heading` is `0` rather than `NaN` when the heading never changes. `circular::sd.circular()` returns `NaN` there, because the resultant length of a constant sample can land above 1 in floating point; anicore's `circ_sd()` clamps it. A keypoint that does not move produces exactly this (animovement/anicore#147).

# animetric 0.5.0 (2026-08-28)

## Changed

* The minimum `anicore` is 0.8.0, the first version published under that name — the dependency was renamed without a version constraint, so nothing recorded that a pre-rename `aniframe` will not do.

* The core data structures come from `anicore`, which is what the `aniframe` package was renamed to in its 0.8.0 (animovement/anicore#84). The `aniframe` class keeps its name; only the package providing it changed.

* The `calculate_kinematics()`, `calculate_nnd()` and `calculate_tortuosity()` examples run rather than sitting in `\dontrun{}`. Each builds its own frame with `anicore::example_aniframe()`; they were wrapped because they referred to an undefined `data`, so they had never been checked against the functions they document.

* `calculate_nnd()` reads the columns it needs from the aniframe's metadata, and takes the identity roles explicitly (#37). It previously hard-coded `individual`, `c("session", "trial", "time")`, `keypoint` and `x`/`y`/`z`, consulting the metadata for none of them.

  That was already producing wrong numbers: `observation` joined `variables_when` in aniframe 0.6.0, but the hard-coded context list never picked it up, so multi-clip data was pooled and each animal could be matched to one in a *different clip* — silently, and with a plausible-looking distance.

  `across` names the column whose value must differ (required — nothing is inferred), `within` names columns that must match on top of the temporal context, and `focal` / `neighbour` restrict which points are measured from and to. Being independent, they express asymmetric questions:

  ```r
  # whose neck is my head nearest to?
  data |> calculate_nnd(
    across = "individual",
    focal = list(keypoint = "head"),
    neighbour = list(keypoint = "neck")
  )

  # nearest keypoint within each animal
  data |> calculate_nnd(across = "keypoint", within = "individual")
  ```

  Existing calls need `across = "individual"` added. `keypoint_neighbour` still works, with a deprecation warning, and maps to `neighbour = list(keypoint = ...)`.

* Frames identified by `track` or `subject` rather than `individual` now work, as do multi-observation frames. Polar, cylindrical and spherical frames error with a pointer to `anispace::map_to_cartesian()` rather than silently measuring in mixed units.

* `compute_nnd()` takes `across`, `is_focal`, `is_candidate` and `labels` in place of `individual`, `keypoint` and `keypoint_neighbour`, mirroring the generalisation above. Its result names the ranked column (`nnd_across`) which `calculate_nnd()` renames.

* `summarise_keypoints()` is renamed `add_centroid()`, and takes `across` to choose the level it collapses (#47). **The old name is gone rather than deprecated** — it has not been in a release, so nothing can be depending on it. The old name said `summarise_`, which in this package means collapsing a frame to summary rows — this appends them. It also named the keypoint level, which is only one of the levels a frame can be summarised across.

  `across` names the identity variables to collapse, so the same question can be asked at any scale. On pose data for a team:

  ```r
  add_centroid(af, across = "keypoint")                  # each player's own centre
  add_centroid(af, across = "individual")                # one centre per keypoint, across players
  add_centroid(af, across = c("individual", "keypoint")) # the point the whole team occupies
  ```

  It is not guessed. `variables_what` is documented coarse to fine, but nothing enforces that and attributes like `sex` or `treatment` do not nest at all, so a frame declaring more than one identity variable has to be told which to collapse (animovement/anicore#140, animovement/anicore#141). A frame declaring one is unambiguous and needs no argument.

  A collapsed level that did not actually vary keeps its value rather than taking the summary's name — an individual's strain is still its strain, since nothing was averaged over it.

  Only identity variables can be collapsed. Collapsing the index or a temporal variable averages over time, which is what the `summarise_*()` family does.

* `compute_centroid()`'s `include_keypoints`, `exclude_keypoints` and `centroid_name` are renamed `include`, `exclude` and `name`, and it takes the same `across` (#47). `add_area` is gone from the summary function: it was never implemented, and area is a different shape of answer that will arrive as its own function rather than a flag.

## Fixed

* `calculate_kinematics()`, `calculate_tortuosity()` and `summarise_tortuosity()` refuse a frame whose grouping pools several trajectories, rather than answering wrongly (#54). Speed and path length come from successive rows within a group, so a group has to hold one position per moment. Regrouping an aniframe coarsely — every keypoint of an animal together, say — made the distance *between* keypoints count as movement:

  ```r
  # two keypoints 100 apart, each drifting 1 per frame
  calculate_kinematics(af)                      # mean speed 1, correct
  calculate_kinematics(regrouped_by_individual) # mean speed 2.7, silently
  ```

  Path length accumulates the same way, and `summarise_tortuosity()` takes its last value minus its first — across concatenated trajectories that is a number describing nothing, and it was inflating totals about threefold.

  Regrouping itself is still allowed; `anicore` already warns that a frame's grouping and its declaration then disagree. This is narrower and firmer: these computations have a precondition, and the error says how to summarise more coarsely — summarise at the declared grouping first, then combine those results.

* `compute_centroid()` carries the source frame's metadata into its result (#38, #47). Sampling rate, units and the rest were dropped, so a centroid arrived claiming to know nothing about the recording it came from.

* A centroid no longer gains a `confidence` column on frames that do not track one (#47).

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

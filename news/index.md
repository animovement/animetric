# Changelog

## animetric (development version)

### Removed

- `mean_angle()` and `median_angle()` are removed. Use
  [`anicore::circ_mean()`](https://animovement.dev/anicore/reference/circ_mean.html)
  and
  [`anicore::circ_median()`](https://animovement.dev/anicore/reference/circ_median.html),
  which are attached by
  [`library(animovement)`](https://rdrr.io/r/base/library.html)
  (animovement/anicore#147). The circular statistics live in one place
  now, and these were the two that had drifted: `mean_angle()`
  duplicated `circ_mean()` exactly, and `median_angle()` was not a
  circular median at all.

  `median_angle()` took the median of the sine and cosine components,
  which is not rotation-equivariant — rotating every angle in a sample
  by the same amount moved its answer by a different amount, so the
  result depended on where the circle was cut.
  [`anicore::circ_median()`](https://animovement.dev/anicore/reference/circ_median.html)
  is Fisher’s circular median and does not have that defect, so it is a
  replacement that returns **different numbers**. Any stored values
  computed with `median_angle()` were frame-dependent.

### Fixed

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  gives correct results for polar, cylindrical and spherical input
  stored in degrees. It converts such input to Cartesian with anispace’s
  `map_to_*()`, which read every angle as radians until
  animovement/anispace#47, so `phi = 90` was taken as 90 radians.
  animetric now requires anispace 0.3.0.9005, which has the fix.

- `heading` from
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  is `pi` for movement along `-x`, where it used to be rewritten to `0`
  — the opposite direction
  ([\#69](https://github.com/animovement/animetric/issues/69)). The
  rewrite fired whenever `v_y` was exactly zero, which integer-pixel
  tracking produces routinely, so a trajectory along `-x` picked up
  180-degree jumps that showed as spikes in `angular_velocity` and
  `angular_acceleration` and as turning in `angular_path_length` that
  never happened. `heading` is now `NA` where speed is zero, since a
  stationary animal has no direction of travel; before, it read as `0`,
  or as `pi` when the velocity was a negative zero.

- `angular_path_length` from
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  starts at `0`
  ([\#68](https://github.com/animovement/animetric/issues/68)). When the
  first heading was negative it started at `2 * abs(heading)` and
  carried that offset along the whole trajectory, so a straight line at
  a heading of `-1` reported two radians of turning.
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  takes the difference between the last and first values, so its
  `total_angular_path_length` was not affected. Turning made while the
  animal is stopped (where `heading` is `NA`) is counted when it moves
  off again.

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md),
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  read the axes and the index from the frame’s declared variables, so
  they work on frames whose columns are not called `x`, `y`, `z` and
  `time` ([\#81](https://github.com/animovement/animetric/issues/81)).
  They used to stop with “Column `x` not found”. Velocity and
  acceleration components are named by axis role (`v_x`, `a_y`, …)
  whatever the input columns are called. 1D frames get translational
  kinematics and tortuosity too;
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  used to return them unchanged. The 2D and 3D code paths are now one
  implementation, with turning angles from
  [`anicore::angle_between()`](https://animovement.dev/anicore/reference/angle_between.html).
  The only change in output is that 3D windowed tortuosity is `NA` at
  the first row where the window is incomplete, as it already was in 2D;
  the old 3D code filled the missing start of the window with the first
  position.

- Angular measures are returned in the frame’s declared `unit_angle`
  ([\#80](https://github.com/animovement/animetric/issues/80)). A frame
  declared in degrees used to get `heading`, `angular_velocity`,
  `angular_acceleration`, `angular_path_length` and the heading
  summaries in radians, still labelled as degrees, so anything that
  trusted the metadata was off by a factor of 180/pi.
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  now report them in degrees for such frames, with no extra step. Radian
  frames are unchanged. The documentation now also says which way signed
  angles run: they follow the frame’s own axes, counter-clockwise when
  `y` points up, as aniread leaves image data.

- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  keeps the input’s metadata, such as `sampling_rate`, and works on
  frames whose axis columns have custom names. It used to rebuild its
  result by re-detecting the columns, which dropped both.

### Added

- [`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md)
  declares which way a body faces from where its points are
  ([\#97](https://github.com/animovement/animetric/issues/97)):
  - **2D:** `heading`, the direction from `from` to `to`.
  - **3D:** a unit quaternion (`qw`, `qx`, `qy`, `qz`), with a third
    point, `plane`, to fix the roll. Any point off the `from`-`to` line
    will do.
  - **Across the body:** `perpendicular = TRUE` handles axes that run
    across the body, such as right eye to left eye.
  - **Where it goes:** `attach_to` puts the orientation on chosen
    members, so a head and a body orientation can share the one declared
    column.

  Once declared, the orientation is a proper `where` variable, which
  anispace’s egocentric transform can align by. Needs anispace
  0.3.0.9006 (`quat_from_vectors()`).

### Changed

- [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md),
  [`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md)
  and the `vertical` argument of
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  are marked experimental (animovement/.github#46). They are new designs
  that have not been used in anger yet, so they may still change without
  a deprecation cycle; anything without a badge is stable, and changes
  only through one.
  [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  has open design questions
  ([\#91](https://github.com/animovement/animetric/issues/91),
  [\#92](https://github.com/animovement/animetric/issues/92),
  [\#22](https://github.com/animovement/animetric/issues/22)), the first
  uses of
  [`add_orientation()`](https://animovement.dev/animetric/reference/add_orientation.md)
  are still being designed
  ([\#25](https://github.com/animovement/animetric/issues/25),
  [\#85](https://github.com/animovement/animetric/issues/85)), and
  `vertical` may come to default to a vertical declared in the frame’s
  metadata (animovement/anicore#172).

- **The summaries are reorganised into two functions, by what they
  summarise**
  ([\#58](https://github.com/animovement/animetric/issues/58)). Sliding
  windows stay in the `calculate_*()` functions; both summaries cover
  each group’s whole time range.

  - [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
    summarises the *distribution* of per-row measures, with one row per
    group and any grouping allowed. It is now an S3 generic:
    - **anipoints:** speed, acceleration, the turning measures, course
      and elevation, the windowed `straightness`, `sinuosity` and
      `emax`, `confidence`, and a declared `yaw` as `*_heading`.
    - **anisegments:** `length`.
    - **anijoints:** `angle`.

    Angles (`course`, `yaw`, joint angles) get circular statistics,
    reported in the frame’s `unit_angle`. `cols =` picks the measures.
    It no longer needs
    [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
    to have been run: it summarises whichever measures the frame has.
  - [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
    measures each trajectory as a whole: `total_path_length`,
    `total_turning`, `net_displacement`, `straightness`, `sinuosity` and
    `emax`. It works on any anipoint in any coordinate system, computing
    what it needs from the positions, and needs one trajectory per
    group.

  `median_straightness` from
  [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md)
  is the typical straightness over windows; `straightness` from
  [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md)
  is how direct the whole route was.

- **Deprecated:**

  - [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
    →
    [`summarise_aniframe()`](https://animovement.dev/animetric/reference/summarise_aniframe.md).
  - [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
    →
    [`summarise_path()`](https://animovement.dev/animetric/reference/summarise_path.md),
    which returns the same columns.
  - `summarise_aniframe(type = )` keeps its old combined output, with a
    warning.

  Each deprecated call returns exactly what it did before. They will be
  removed after the next release.

- [`add_point()`](https://animovement.dev/animetric/reference/add_point.md)
  and
  [`compute_point()`](https://animovement.dev/animetric/reference/compute_point.md)
  replace
  [`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md)
  and
  [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md)
  ([\#94](https://github.com/animovement/animetric/issues/94)). They
  derive a new member of an identity level at each moment, as before,
  and `method` now chooses how:

  - `"centroid"`, the mean (the default, and what the old functions
    did);
  - `"median"`, robust to a stray keypoint;
  - `"weighted"`, weighted by `confidence`;
  - a function of your own, e.g. `\(x) mean(x, trim = 0.1)`.

  A declared orientation is derived for the new member too: the circular
  mean of `yaw`, or the mean quaternion in 3D. It used to be left `NA`.
  [`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md)
  and
  [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md)
  are deprecated, and return exactly what they did.

- **The `aniframe_kin` class is retired**
  ([\#58](https://github.com/animovement/animetric/issues/58)).
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  returns a plain anipoint. The class only labelled a frame as having
  been through
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  recorded nothing about its columns, and survived `select(-speed)`.
  [`is_aniframe_kin()`](https://animovement.dev/animetric/reference/is_aniframe_kin.md)
  is deprecated; check for the columns you need instead. Declaring
  derived columns in anicore’s metadata is the planned replacement
  (animovement/anicore#174).

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  gives turning measures for 3D data too
  ([\#63](https://github.com/animovement/animetric/issues/63)). Every 3D
  frame gets `turning_speed` (how fast the direction of travel turns, in
  any direction) and `cumulative_turning`, and
  `calculate_kinematics(data, vertical = "z")` adds `course` and
  `course_unwrapped` in the horizontal plane, `course_elevation` (the
  angle of travel above it), and the signed horizontal `turning_rate`
  and `turning_acceleration`. The new `vertical` argument names the axis
  that points up in the world, with a minus sign when it points down
  (`"-y"`); the frame’s `axis_directions` cannot supply it, since they
  are relative to the camera (animovement/anicore#172 proposes declaring
  it). Course counts about the vertical by the right-hand rule. The 3D
  turning speed is the angle between the velocities either side of each
  row over the time between them, so it has no wrap at +/-pi and no
  singularity when travel is vertical; 2D results are unchanged.
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  summarise the new columns, and `total_turning` is now reported for 3D.

- **Breaking:** the velocity-derived angular columns are renamed to say
  they describe the path, not the body
  ([\#70](https://github.com/animovement/animetric/issues/70)). Course
  is the direction of travel and turning rate is how fast it changes,
  while heading and angular velocity are where the animal faces and how
  fast that turns. They differ for any animal that does not move
  nose-first, and the orientation names are kept free for body
  orientation once anicore records it (animovement/anicore#46).

  | Old                    | New                    |
  |------------------------|------------------------|
  | `heading`              | `course`               |
  | `heading_unwrapped`    | `course_unwrapped`     |
  | `angular_velocity`     | `turning_rate`         |
  | `angular_speed`        | `turning_speed`        |
  | `angular_acceleration` | `turning_acceleration` |
  | `angular_path_length`  | `cumulative_turning`   |

  The summaries follow: `median_heading`, `mad_heading`, `mean_heading`
  and `sd_heading` become `*_course`, `*_angular_speed`,
  `*_angular_velocity` and `*_angular_acceleration` become
  `*_turning_speed`, `*_turning_rate` and `*_turning_acceleration`, and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)’s
  `total_angular_path_length` becomes `total_turning`. Values are
  unchanged.

- data.table is now a hard dependency
  ([\#27](https://github.com/animovement/animetric/issues/27)).
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  cannot work without it, so the first call on a fresh install used to
  stop and offer to install it.

- Works with anicore’s `anipoint` class and rebuilt accessor API
  (animovement/anicore#154).
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md)
  returns a frame of class
  `c("aniframe_kin", "anipoint", "aniframe", ...)`.

- The circular summaries in
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  come from anicore, and the `circular` package is no longer needed at
  all (animovement/anicore#147). It was a soft dependency behind a
  `check_installed()` prompt, so the first call to
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  on a fresh install used to stop and ask to install a package — for two
  columns of the summary table.

- `mean_heading` is reported in `[0, 2*pi)`, like `median_heading`
  already was. It previously came back in `(-pi, pi]`, so the two
  summaries of the same column disagreed about where the circle starts;
  near `+/-pi` that showed up as a mean of `-3.13` beside a median of
  `3.15`. Both now use the range
  [`anicore::wrap_angle()`](https://animovement.dev/anicore/reference/wrap_angle.html)
  gives by default. The direction is unchanged — only how it is written
  down.

### Fixed

- **`median_heading` could be 180 degrees wrong**, and is now correct
  (animovement/anicore#147). Where two directions tie for the circular
  median, the old implementation averaged them arithmetically — it read
  the tied pair out of an undocumented attribute of
  `circular::median.circular()`’s return value and called
  [`mean()`](https://rdrr.io/r/base/mean.html) on it. When the tie
  straddles zero, the arithmetic mean of the two is their antipode.
  Headings tied at 0.1 and 5.8 radians gave 2.95 radians, or 169
  degrees, where the answer is 349 degrees:

  ``` r

  # the two tied directions, and what each way of averaging them gives
  #   arithmetic: (0.1 + 5.8) / 2 = 2.95   -> 169 degrees, the antipode
  #   circular:   anicore::circ_median()   -> 349 degrees
  ```

  Nothing signalled it: no `NA`, no warning, and a plausible direction.
  Heading distributions straddle zero routinely, so this was not a
  corner case.
  [`anicore::circ_median()`](https://animovement.dev/anicore/reference/circ_median.html)
  averages tied directions on the circle, and a grid search over the
  definition — the direction minimising the summed angular distance —
  confirms which of the two is the median. Any stored `median_heading`
  may need recomputing.

- `sd_heading` is `0` rather than `NaN` when the heading never changes.
  `circular::sd.circular()` returns `NaN` there, because the resultant
  length of a constant sample can land above 1 in floating point;
  anicore’s `circ_sd()` clamps it. A keypoint that does not move
  produces exactly this (animovement/anicore#147).

## animetric 0.5.0 (2026-08-28)

### Changed

- The minimum `anicore` is 0.8.0, the first version published under that
  name — the dependency was renamed without a version constraint, so
  nothing recorded that a pre-rename `aniframe` will not do.

- The core data structures come from `anicore`, which is what the
  `aniframe` package was renamed to in its 0.8.0
  (animovement/anicore#84). The `aniframe` class keeps its name; only
  the package providing it changed.

- The
  [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  and
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  examples run rather than sitting in `\dontrun{}`. Each builds its own
  frame with
  [`anicore::example_aniframe()`](https://animovement.dev/anicore/reference/anicore-deprecated.html);
  they were wrapped because they referred to an undefined `data`, so
  they had never been checked against the functions they document.

- [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  reads the columns it needs from the aniframe’s metadata, and takes the
  identity roles explicitly
  ([\#37](https://github.com/animovement/animetric/issues/37)). It
  previously hard-coded `individual`, `c("session", "trial", "time")`,
  `keypoint` and `x`/`y`/`z`, consulting the metadata for none of them.

  That was already producing wrong numbers: `observation` joined
  `variables_when` in aniframe 0.6.0, but the hard-coded context list
  never picked it up, so multi-clip data was pooled and each animal
  could be matched to one in a *different clip* — silently, and with a
  plausible-looking distance.

  `across` names the column whose value must differ (required — nothing
  is inferred), `within` names columns that must match on top of the
  temporal context, and `focal` / `neighbour` restrict which points are
  measured from and to. Being independent, they express asymmetric
  questions:

  ``` r

  # whose neck is my head nearest to?
  data |> calculate_nnd(
    across = "individual",
    focal = list(keypoint = "head"),
    neighbour = list(keypoint = "neck")
  )

  # nearest keypoint within each animal
  data |> calculate_nnd(across = "keypoint", within = "individual")
  ```

  Existing calls need `across = "individual"` added.
  `keypoint_neighbour` still works, with a deprecation warning, and maps
  to `neighbour = list(keypoint = ...)`.

- Frames identified by `track` or `subject` rather than `individual` now
  work, as do multi-observation frames. Polar, cylindrical and spherical
  frames error with a pointer to
  [`anispace::map_to_cartesian()`](https://animovement.dev/anispace/reference/map_to_cartesian.html)
  rather than silently measuring in mixed units.

- [`compute_nnd()`](https://animovement.dev/animetric/reference/compute_nnd.md)
  takes `across`, `is_focal`, `is_candidate` and `labels` in place of
  `individual`, `keypoint` and `keypoint_neighbour`, mirroring the
  generalisation above. Its result names the ranked column
  (`nnd_across`) which
  [`calculate_nnd()`](https://animovement.dev/animetric/reference/calculate_nnd.md)
  renames.

- `summarise_keypoints()` is renamed
  [`add_centroid()`](https://animovement.dev/animetric/reference/add_centroid.md),
  and takes `across` to choose the level it collapses
  ([\#47](https://github.com/animovement/animetric/issues/47)). **The
  old name is gone rather than deprecated** — it has not been in a
  release, so nothing can be depending on it. The old name said
  `summarise_`, which in this package means collapsing a frame to
  summary rows — this appends them. It also named the keypoint level,
  which is only one of the levels a frame can be summarised across.

  `across` names the identity variables to collapse, so the same
  question can be asked at any scale. On pose data for a team:

  ``` r

  add_centroid(af, across = "keypoint")                  # each player's own centre
  add_centroid(af, across = "individual")                # one centre per keypoint, across players
  add_centroid(af, across = c("individual", "keypoint")) # the point the whole team occupies
  ```

  It is not guessed. `variables_what` is documented coarse to fine, but
  nothing enforces that and attributes like `sex` or `treatment` do not
  nest at all, so a frame declaring more than one identity variable has
  to be told which to collapse (animovement/anicore#140,
  animovement/anicore#141). A frame declaring one is unambiguous and
  needs no argument.

  A collapsed level that did not actually vary keeps its value rather
  than taking the summary’s name — an individual’s strain is still its
  strain, since nothing was averaged over it.

  Only identity variables can be collapsed. Collapsing the index or a
  temporal variable averages over time, which is what the
  `summarise_*()` family does.

- [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md)’s
  `include_keypoints`, `exclude_keypoints` and `centroid_name` are
  renamed `include`, `exclude` and `name`, and it takes the same
  `across` ([\#47](https://github.com/animovement/animetric/issues/47)).
  `add_area` is gone from the summary function: it was never
  implemented, and area is a different shape of answer that will arrive
  as its own function rather than a flag.

### Fixed

- [`calculate_kinematics()`](https://animovement.dev/animetric/reference/calculate_kinematics.md),
  [`calculate_tortuosity()`](https://animovement.dev/animetric/reference/calculate_tortuosity.md)
  and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  refuse a frame whose grouping pools several trajectories, rather than
  answering wrongly
  ([\#54](https://github.com/animovement/animetric/issues/54)). Speed
  and path length come from successive rows within a group, so a group
  has to hold one position per moment. Regrouping an aniframe coarsely —
  every keypoint of an animal together, say — made the distance
  *between* keypoints count as movement:

  ``` r

  # two keypoints 100 apart, each drifting 1 per frame
  calculate_kinematics(af)                      # mean speed 1, correct
  calculate_kinematics(regrouped_by_individual) # mean speed 2.7, silently
  ```

  Path length accumulates the same way, and
  [`summarise_tortuosity()`](https://animovement.dev/animetric/reference/summarise_tortuosity.md)
  takes its last value minus its first — across concatenated
  trajectories that is a number describing nothing, and it was inflating
  totals about threefold.

  Regrouping itself is still allowed; `anicore` already warns that a
  frame’s grouping and its declaration then disagree. This is narrower
  and firmer: these computations have a precondition, and the error says
  how to summarise more coarsely — summarise at the declared grouping
  first, then combine those results.

- [`compute_centroid()`](https://animovement.dev/animetric/reference/compute_centroid.md)
  carries the source frame’s metadata into its result
  ([\#38](https://github.com/animovement/animetric/issues/38),
  [\#47](https://github.com/animovement/animetric/issues/47)). Sampling
  rate, units and the rest were dropped, so a centroid arrived claiming
  to know nothing about the recording it came from.

- A centroid no longer gains a `confidence` column on frames that do not
  track one
  ([\#47](https://github.com/animovement/animetric/issues/47)).

### Removed

- The re-exports of `as_aniframe()`, `is_aniframe()`,
  `ensure_is_aniframe()`, `deg_to_rad()`, `rad_to_deg()`,
  `wrap_angle()`, `unwrap_angle()`, `calculate_angular_difference()` and
  `diff_angle()`. **Calls to these through `animetric::` need repointing
  at `anicore::` or `anispace::`.** animetric still uses them internally
  — it just has no reason to publish another package’s interface as its
  own, which left the same function documented in two places and
  animetric’s exports growing whenever anicore’s did.

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
- Circular statistics: `mean_angle()` and `median_angle()`.
- Summaries:
  [`summarise_kinematics()`](https://animovement.dev/animetric/reference/summarise_kinematics.md)
  and `summarise_keypoints()`.
- [`is_aniframe_kin()`](https://animovement.dev/animetric/reference/is_aniframe_kin.md)
  to test whether a frame carries kinematics.
- Angle helpers re-exported from aniframe: `deg_to_rad()`,
  `rad_to_deg()`, `wrap_angle()`, `unwrap_angle()`, `diff_angle()` and
  `calculate_angular_difference()`.

## animetric 0.1.0

First commit. animetric computes movement metrics from an aniframe —
kinematics, path complexity and summaries over a trajectory.

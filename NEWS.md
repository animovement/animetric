# animetric (development version)

## Breaking changes

* `calculate_nnd()` now reads the columns it needs from the aniframe's metadata, and takes the identity roles explicitly (#37). It previously hard-coded `individual`, `c("session", "trial", "time")`, `keypoint` and `x`/`y`/`z`, consulting the metadata for none of them.

  That was already producing wrong numbers: `observation` joined `variables_when` in aniframe 0.6.0, but the hard-coded context list never picked it up, so multi-clip data was pooled and each animal could be matched to one in a *different clip* — silently, and with a plausible-looking distance.

  `across` names the column whose value must differ (required — nothing is inferred), `within` names columns that must match on top of the temporal context, and `focal` / `neighbour` restrict which points are measured from and to. Being independent, they express asymmetric questions:

  ```r
  # whose tail is my nose nearest to?
  data |> calculate_nnd(
    across = "individual",
    focal = list(keypoint = "nose"),
    neighbour = list(keypoint = "tail")
  )

  # nearest keypoint within each animal
  data |> calculate_nnd(across = "keypoint", within = "individual")
  ```

  Existing calls need `across = "individual"` added. `keypoint_neighbour` still works, with a deprecation warning, and maps to `neighbour = list(keypoint = ...)`.

* Frames identified by `track` or `subject` rather than `individual` now work, as do multi-observation frames. Polar, cylindrical and spherical frames error with a pointer to `anispace::map_to_cartesian()` rather than silently measuring in mixed units.

* `compute_nnd()` takes `across`, `is_focal`, `is_candidate` and `labels` in place of `individual`, `keypoint` and `keypoint_neighbour`, mirroring the generalisation above. Its result names the ranked column (`nnd_across`) which `calculate_nnd()` renames.

## Internal

* Documentation regenerated with roxygen2 8.1.0, matching the rest of the ecosystem. This restyles the `importFrom` block in `NAMESPACE`, renames `RoxygenNote` to `Config/roxygen2/version` in `DESCRIPTION`, links re-exports by topic rather than by name, and picks up the co-author entry that had been missing from the package doc page.

## Bug fixes

* `calculate_nnd()` no longer warns "Unknown or uninitialised column" on frames without a `keypoint` column, and reports an absent `individual` column as absent rather than as all-`NA`. Both came from reading a column with `data$col` before checking it exists: on a frame without it, that returns `NULL` with a warning, and `all(is.na(NULL))` is `TRUE`, so the two cases were indistinguishable. Surfaced by aniframe 0.6.0.9005, which stopped adding a `keypoint` column beside an existing identity (animovement/aniframe#77) — the eleven warnings the test suite had been emitting are gone.

## Breaking changes

* Removed the `aniframe_kin2d` and `aniframe_kin3d` classes. `calculate_kinematics()` set them, but nothing ever read them — no predicate, no method, no test in any package — and the dimensionality they encoded is already carried by the `coordinate_system` metadata field, so they were a second source of truth that could only ever disagree with the first. Kinematics output still carries `aniframe_kin`, which is the class the `summarise_*()` functions dispatch on.

# animetric 0.3.0

This release brings a re-factoring of the `calculate` and `summarise` functions, along with new `tortuosity` metrics and a bucnh of tests. 
I'm also excited to bring on the first social/collective metric: "Nearest Neighbour Distance" (NND). The calculations are made thoughout the time series with `calculate_nnd()`.
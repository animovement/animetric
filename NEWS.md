# animetric (development version)

## Internal

* Documentation regenerated with roxygen2 8.1.0, matching the rest of the ecosystem. This restyles the `importFrom` block in `NAMESPACE`, renames `RoxygenNote` to `Config/roxygen2/version` in `DESCRIPTION`, links re-exports by topic rather than by name, and picks up the co-author entry that had been missing from the package doc page.

## Bug fixes

* `calculate_nnd()` no longer warns "Unknown or uninitialised column" on frames without a `keypoint` column, and reports an absent `individual` column as absent rather than as all-`NA`. Both came from reading a column with `data$col` before checking it exists: on a frame without it, that returns `NULL` with a warning, and `all(is.na(NULL))` is `TRUE`, so the two cases were indistinguishable. Surfaced by aniframe 0.6.0.9005, which stopped adding a `keypoint` column beside an existing identity (animovement/aniframe#77) — the eleven warnings the test suite had been emitting are gone.

## Breaking changes

* Removed the `aniframe_kin2d` and `aniframe_kin3d` classes. `calculate_kinematics()` set them, but nothing ever read them — no predicate, no method, no test in any package — and the dimensionality they encoded is already carried by the `coordinate_system` metadata field, so they were a second source of truth that could only ever disagree with the first. Kinematics output still carries `aniframe_kin`, which is the class the `summarise_*()` functions dispatch on.

# animetric 0.3.0

This release brings a re-factoring of the `calculate` and `summarise` functions, along with new `tortuosity` metrics and a bucnh of tests. 
I'm also excited to bring on the first social/collective metric: "Nearest Neighbour Distance" (NND). The calculations are made thoughout the time series with `calculate_nnd()`.
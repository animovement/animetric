# animetric (development version)

## Breaking changes

* Removed the `aniframe_kin2d` and `aniframe_kin3d` classes. `calculate_kinematics()` set them, but nothing ever read them — no predicate, no method, no test in any package — and the dimensionality they encoded is already carried by the `coordinate_system` metadata field, so they were a second source of truth that could only ever disagree with the first. Kinematics output still carries `aniframe_kin`, which is the class the `summarise_*()` functions dispatch on.

# animetric 0.3.0

This release brings a re-factoring of the `calculate` and `summarise` functions, along with new `tortuosity` metrics and a bucnh of tests. 
I'm also excited to bring on the first social/collective metric: "Nearest Neighbour Distance" (NND). The calculations are made thoughout the time series with `calculate_nnd()`.
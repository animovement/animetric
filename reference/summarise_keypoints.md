# Summarize keypoint data

Creates summary statistics across multiple keypoints at each time point.
Currently supports computing centroids from selected keypoints. Future
functionality will include polygonal summaries.

## Usage

``` r
summarise_keypoints(
  data,
  keypoints = "all",
  name = "centroid",
  add_area = FALSE
)
```

## Arguments

- data:

  An aniframe containing keypoint data.

- keypoints:

  Character vector of keypoint names to summarize, or "all" to use all
  keypoints in the data. Default is "all".

- name:

  Character string for the name of the new summary keypoint. Default is
  "centroid".

- add_area:

  Logical indicating whether to compute area (not yet implemented).
  Default is FALSE.

## Value

An aniframe with the original data plus the new summary keypoint.

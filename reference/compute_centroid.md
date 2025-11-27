# Compute centroid from keypoints

Calculates the mean position of selected keypoints at each time point.
The centroid is computed for each combination of grouping variables
(individual, time, trial/session if present).

## Usage

``` r
compute_centroid(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
)
```

## Arguments

- data:

  An aniframe with Cartesian coordinates (x, y, and/or z columns).

- include_keypoints:

  Character vector of keypoints to include in centroid calculation. If
  NULL (default), all keypoints are used unless `exclude_keypoints` is
  specified. Mutually exclusive with `exclude_keypoints`.

- exclude_keypoints:

  Character vector of keypoints to exclude from centroid calculation. If
  NULL (default), no keypoints are excluded. Mutually exclusive with
  `include_keypoints`.

- centroid_name:

  Name for the new centroid keypoint. Default is "centroid".

## Value

An aniframe containing only the centroid keypoint. Coordinate values are
the mean of selected keypoints (with NA values removed). Confidence is
set to NA. Missing coordinate dimensions return NA.

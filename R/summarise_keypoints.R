#' Summarize keypoint data
#'
#' Creates summary statistics across multiple keypoints at each time point.
#' Currently supports computing centroids from selected keypoints. Future
#' functionality will include polygonal summaries.
#'
#' @param data An aniframe containing keypoint data.
#' @param keypoints Character vector of keypoint names to summarize, or "all"
#'   to use all keypoints in the data. Default is "all".
#' @param name Character string for the name of the new summary keypoint.
#'   Default is "centroid".
#' @param add_area Logical indicating whether to compute area (not yet
#'   implemented). Default is FALSE.
#'
#' @return An aniframe with the original data plus the new summary keypoint.
#'
#' @export
summarise_keypoints <- function(
  data,
  keypoints = "all",
  name = "centroid",
  add_area = FALSE
) {
  # Validate input
  aniframe::ensure_is_aniframe(data)

  # Resolve keypoint selection
  if (identical(keypoints, "all")) {
    keypoints <- unique(data$keypoint)
  }

  # Validate name doesn't conflict
  if (name %in% keypoints) {
    cli::cli_abort(
      "Summary name {.val {name}} conflicts with existing keypoint."
    )
  }

  # Check minimum keypoints requirement
  n_keypoints <- length(keypoints)
  if (n_keypoints < 2) {
    cli::cli_abort(
      "At least 2 keypoints required for summarization, found {n_keypoints}."
    )
  }

  # Identify grouping variables
  grps <- attr(data, "groups") |>
    names() |>
    setdiff(c(".rows", "keypoint")) |>
    c("time")

  # Compute summary statistics
  # TODO: Add polygon computation and area calculation
  # TODO: Handle n_keypoints == 2 case differently if needed
  md <- aniframe::get_metadata(data)

  data |>
    dplyr::bind_rows(
      compute_centroid(
        data,
        include_keypoints = keypoints,
        centroid_name = name
      )
    ) |>
    aniframe::as_aniframe() |>
    aniframe::set_metadata(metadata = md)
}

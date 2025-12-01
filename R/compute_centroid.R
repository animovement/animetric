#' Compute centroid from keypoints
#'
#' Calculates the mean position of selected keypoints at each time point.
#' The centroid is computed for each combination of grouping variables
#' (individual, time, trial/session if present).
#'
#' @param data An aniframe with Cartesian coordinates (x, y, and/or z columns).
#' @param include_keypoints Character vector of keypoints to include in centroid
#'   calculation. If NULL (default), all keypoints are used unless
#'   `exclude_keypoints` is specified. Mutually exclusive with `exclude_keypoints`.
#' @param exclude_keypoints Character vector of keypoints to exclude from centroid
#'   calculation. If NULL (default), no keypoints are excluded. Mutually exclusive
#'   with `include_keypoints`.
#' @param centroid_name Name for the new centroid keypoint. Default is "centroid".
#'
#' @return An aniframe containing only the centroid keypoint. Coordinate values
#'   are the mean of selected keypoints (with NA values removed). Confidence is
#'   set to NA. Missing coordinate dimensions return NA.
#'
#' @export
compute_centroid <- function(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
) {
  # Validate input
  aniframe::ensure_is_aniframe(data)

  if (!aniframe::is_cartesian(data)) {
    cli::cli_abort("Data must be in a Cartesian coordinate system.")
  }

  if (!is.null(include_keypoints) && !is.null(exclude_keypoints)) {
    cli::cli_abort(
      "Cannot specify both {.arg include_keypoints} and {.arg exclude_keypoints}."
    )
  }

  # Filter keypoints
  if (!is.null(include_keypoints)) {
    data <- dplyr::filter(data, .data$keypoint %in% include_keypoints)
  } else if (!is.null(exclude_keypoints)) {
    data <- dplyr::filter(data, !.data$keypoint %in% exclude_keypoints)
  }

  # Determine available coordinates
  has_x <- "x" %in% names(data)
  has_y <- "y" %in% names(data)
  has_z <- "z" %in% names(data)

  # Calculate centroid
  centroid <- data |>
    dplyr::ungroup(.data$keypoint) |>
    dplyr::group_by(.data$time, .add = TRUE) |>
    dplyr::summarise(
      x = if (has_x) mean(.data$x, na.rm = TRUE) else NA_real_,
      y = if (has_y) mean(.data$y, na.rm = TRUE) else NA_real_,
      z = if (has_z) mean(.data$z, na.rm = TRUE) else NA_real_,
      confidence = NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(keypoint = factor(centroid_name)) |>
    aniframe::convert_nan_to_na() |>
    suppressMessages() |>
    suppressWarnings()

  # Remove z column if not in original data
  if (!has_z) {
    centroid <- dplyr::select(centroid, -"z")
  }

  aniframe::as_aniframe(centroid)
}

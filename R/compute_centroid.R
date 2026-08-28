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
#' @examples
#' af <- anicore::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 3)
#' compute_centroid(af)
#'
#' # A centroid from a subset of keypoints
#' compute_centroid(af, include_keypoints = c("head", "neck"))
#' @export
compute_centroid <- function(
  data,
  include_keypoints = NULL,
  exclude_keypoints = NULL,
  centroid_name = "centroid"
) {
  # Validate input
  anicore::ensure_is_aniframe(data)

  if (!anicore::is_cartesian(data)) {
    cli::cli_abort("Data must be in a Cartesian coordinate system.")
  }

  if (!is.null(include_keypoints) && !is.null(exclude_keypoints)) {
    cli::cli_abort(
      "Cannot specify both {.arg include_keypoints} and {.arg exclude_keypoints}."
    )
  }

  # Identity, position and the index all come from the frame's declaration.
  # A valid aniframe may carry them in columns named anything (#47).
  identity_col <- finest_identity(data)
  space_cols <- anicore::get_variables_where(data)
  keep <- retained_grouping(data, identity_col)

  # Filter keypoints
  if (!is.null(include_keypoints)) {
    data <- dplyr::filter(data, .data[[identity_col]] %in% include_keypoints)
  } else if (!is.null(exclude_keypoints)) {
    data <- dplyr::filter(data, !.data[[identity_col]] %in% exclude_keypoints)
  }

  # Calculate centroid
  centroid <- data |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keep))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(space_cols),
        \(v) mean(v, na.rm = TRUE)
      ),
      confidence = NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(!!identity_col := factor(centroid_name)) |>
    anicore::convert_nan_to_na() |>
    suppressMessages() |>
    suppressWarnings()

  # Re-declare rather than re-detect. Detection only recognises the standard
  # identity names, so a frame using its own would have a `keypoint` column
  # injected and its declaration replaced (#47).
  anicore::as_aniframe(
    centroid,
    variables_what = anicore::get_variables_what(data),
    variables_when = anicore::get_variables_when(data),
    variables_where = space_cols,
    index = anicore::get_index(data)
  )
}

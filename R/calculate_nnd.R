#' Calculate distance to n-th nearest neighbour
#'
#' Computes the distance from each point to the n-th nearest point belonging
#' to a different individual. Optionally filter by keypoint on neighbouring
#' individuals.
#'
#' @param data An aniframe with x, y (and optionally z) coordinates and
#'   individual identifiers.
#' @param n Which neighbour (1 = nearest, 2 = second nearest, etc.).
#' @param keypoint_neighbour Keypoint(s) on other individuals to consider as
#'   potential neighbours. Can be a single keypoint or a character vector. If
#'   NULL (default), considers all keypoints.
#'
#' @return The input aniframe with additional columns:
#'   - `nnd_distance`: distance to the n-th nearest neighbour
#'   - `nnd_individual`: individual ID of the n-th nearest neighbour
#'   - `nnd_keypoint`: keypoint of the neighbour (if keypoint column
#'     exists)
#'
#' @examples
#' data <- anicore::example_aniframe(n_obs = 10, n_individuals = 3, n_keypoints = 3)
#'
#' # Distance to nearest individual (any keypoint to any keypoint)
#' data |> calculate_nnd()
#'
#' # Distance to nearest head of another individual
#' data |> calculate_nnd(keypoint_neighbour = "head")
#'
#' # Head-to-head distance
#' data |>
#'   calculate_nnd(keypoint_neighbour = "head") |>
#'   dplyr::filter(keypoint == "head")
#'
#' # Minimum distance between individuals (across all keypoints)
#' data |>
#'   calculate_nnd() |>
#'   dplyr::group_by(session, trial, time, individual) |>
#'   dplyr::slice_min(nnd_distance, n = 1)
#'
#' @export
calculate_nnd <- function(data, n = 1L, keypoint_neighbour = NULL) {
  # Test for the column before reading it. `data$individual` on a frame
  # without that column returns NULL *with a warning*, and
  # `all(is.na(NULL))` is TRUE, so an absent column was reported as an
  # all-NA one -- and warned on the way.
  if (!"individual" %in% names(data)) {
    cli::cli_abort(c(
      "Column {.var individual} is required but not found in the data.",
      "i" = "Nearest neighbour distances are measured between individuals."
    ))
  }

  if (all(is.na(data$individual))) {
    cli::cli_abort(
      "Column {.var individual} contains only {.val NA} values. At least two
      distinct individuals are required to compute nearest neighbour distances."
    )
  }

  context_cols <- c("session", "trial", "time")
  context_cols <- context_cols[context_cols %in% names(data)]

  if (length(context_cols) == 0) {
    cli::cli_abort(
      "At least one context column ({.var session}, {.var trial}, or
      {.var time}) is required."
    )
  }

  # A frame need not carry a keypoint column at all -- aniframe adds one
  # only when the data declares no identity of its own.
  has_keypoint_col <- "keypoint" %in% names(data) && !all(is.na(data$keypoint))

  if (!is.null(keypoint_neighbour)) {
    if (!has_keypoint_col) {
      reason <- if ("keypoint" %in% names(data)) {
        "Column {.var keypoint} contains only {.val NA} values."
      } else {
        "The data has no {.var keypoint} column."
      }
      cli::cli_abort(c(
        "{.arg keypoint_neighbour} was given, but there are no keypoints to match it against.",
        "x" = reason
      ))
    }

    available_keypoints <- unique(as.character(data$keypoint[
      !is.na(data$keypoint)
    ]))
    missing_keypoints <- setdiff(keypoint_neighbour, available_keypoints)

    if (length(missing_keypoints) == length(keypoint_neighbour)) {
      cli::cli_abort(
        "None of the requested keypoints ({.val {keypoint_neighbour}}) are
        present in the data. Available keypoints: {.val {available_keypoints}}."
      )
    } else if (length(missing_keypoints) > 0) {
      cli::cli_warn(
        "Some requested keypoints are not present in the data:
        {.val {missing_keypoints}}. Using available keypoints:
        {.val {intersect(keypoint_neighbour, available_keypoints)}}."
      )
    }
  }

  incoming_classes <- class(data)

  is_3d <- anicore::is_cartesian_3d(data)

  result <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(context_cols))) |>
    dplyr::group_modify(
      ~ {
        nnd_result <- compute_nnd(
          .x$x,
          .x$y,
          z = if (is_3d) .x$z else NULL,
          individual = .x$individual,
          keypoint = if (has_keypoint_col) .x$keypoint else NULL,
          n = n,
          keypoint_neighbour = keypoint_neighbour
        )
        dplyr::bind_cols(.x, nnd_result)
      }
    ) |>
    dplyr::ungroup() |>
    suppressWarnings() |>
    anicore::as_aniframe()

  outgoing_classes <- class(result)
  class(result) <- c(
    incoming_classes[!incoming_classes %in% outgoing_classes],
    outgoing_classes
  )

  result
}

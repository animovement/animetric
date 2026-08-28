#' Summarize keypoint data
#'
#' @description
#' Renamed [add_centroid()]. The old name said `summarise_`, which in this
#' package means collapsing a frame to summary rows — but this appends rows
#' and returns the input as well. It also named the keypoint level, which is
#' only one of the levels a frame may be summarised across.
#'
#' @param data An aniframe containing keypoint data.
#' @param keypoints Character vector of keypoint names to summarize, or "all"
#'   to use all keypoints in the data.
#' @param name Character string for the name of the new summary keypoint.
#' @param add_area Ignored. Area was never implemented; it will arrive as its
#'   own function rather than a flag on this one.
#'
#' @return An aniframe with the original data plus the new summary keypoint.
#'
#' @examples
#' af <- anicore::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 3)
#' add_centroid(af, across = "keypoint")
#' @export
#' @aliases summarize_keypoints
summarise_keypoints <- function(
  data,
  keypoints = "all",
  name = "centroid",
  add_area = FALSE
) {
  cli::cli_warn(c(
    "{.fn summarise_keypoints} is deprecated.",
    "i" = "Use {.fn add_centroid}, which takes {.arg across} to choose the level being summarised."
  ))

  # The old function always collapsed the finest identity, so the shim keeps
  # doing that rather than making previously-working code error.
  include <- if (identical(keypoints, "all")) NULL else keypoints
  add_centroid(
    data,
    across = finest_identity(data),
    include = include,
    name = name
  )
}

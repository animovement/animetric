# One trajectory per group (#54)
#
# Speed, path length and everything derived from them come from successive
# rows within a group. They mean something only where a group holds one
# position per moment -- which is what the frame's declaration gives, and
# what a regrouped frame may not.

#' Is this frame grouped as one trajectory per group?
#'
#' Speed comes from successive rows within a group, so a group has to hold
#' one position per moment. Pool several keypoints of an animal together and
#' the distance between keypoints is measured as movement: two keypoints
#' 100 apart, each drifting 1 per frame, report a speed of 2.7 instead of 1.
#'
#' Path length accumulates the same way, and the tortuosity summary takes
#' its last value minus its first -- across concatenated trajectories, that
#' is a number describing nothing.
#'
#' Regrouping an aniframe is allowed, and `anicore` warns that its grouping
#' and its declaration then disagree. This is a stronger statement about a
#' narrower thing: these computations have a precondition, and silently
#' returning a wrong number is worse than refusing.
#'
#' @param data An aniframe.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_trajectory_grouping <- function(data, call = rlang::caller_env()) {
  declared <- unique(c(
    anicore::get_variables_what(data),
    anicore::get_variables_when(data)
  ))
  grouping <- dplyr::group_vars(data)

  if (setequal(grouping, declared)) {
    return(invisible(TRUE))
  }

  cli::cli_abort(
    c(
      "This needs one trajectory per group.",
      "i" = "The frame is grouped by {.val {grouping}} and declares {.val {declared}}.",
      "i" = "Speed and path length come from successive rows, so pooling several trajectories measures the distance between them as movement.",
      "i" = "To summarise more coarsely, summarise at the declared grouping first and combine those results."
    ),
    call = call
  )
}

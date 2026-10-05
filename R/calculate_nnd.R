#' Calculate distance to the n-th nearest neighbour
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [add_nnd()], which takes the same arguments apart from
#' `keypoint_neighbour`: functions that return the frame with columns added
#' now start with `add_`. This returns exactly what it did, with the
#' columns named without the neighbour rank that [add_nnd()] puts in them:
#' `nnd_<across>` and `nnd_distance` rather than `nnd_<n>_<across>` and
#' `nnd_<n>_distance`.
#'
#' @param data,across,n,within,focal,neighbour See [add_nnd()].
#' @param keypoint_neighbour Deprecated. Use
#'   `neighbour = list(keypoint = ...)`.
#'
#' @return As [add_nnd()], with the columns it adds named `nnd_distance`,
#'   `nnd_<across>` and `nnd_<variable>`.
#' @keywords internal
#' @export
calculate_nnd <- function(
  data,
  across,
  n = 1L,
  within = NULL,
  focal = NULL,
  neighbour = NULL,
  keypoint_neighbour = NULL
) {
  lifecycle::deprecate_warn("0.6.0", "calculate_nnd()", "add_nnd()")
  anicore::ensure_is_anipoint(data)

  if (!is.null(keypoint_neighbour)) {
    cli::cli_warn(c(
      "{.arg keypoint_neighbour} is deprecated.",
      "i" = "Use {.code neighbour = list(keypoint = ...)} instead."
    ))
    neighbour <- neighbour %||% list(keypoint = keypoint_neighbour)
  }

  result <- add_nnd(
    data,
    across = across,
    n = n,
    within = within,
    focal = focal,
    neighbour = neighbour
  )
  added <- setdiff(names(result), names(data))
  dplyr::rename_with(
    result,
    function(nm) sub("^nnd_\\d+_", "nnd_", nm),
    .cols = dplyr::all_of(added)
  )
}

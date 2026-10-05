#' The minimum step `"auto"` chooses for each trajectory
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Returns the threshold that `min_step = "auto"` uses in [add_kinematics()]
#' and [summarise_path()], one per trajectory, with the positional noise it
#' is estimated from. Use it to see whether the automatic threshold suits
#' your data, to report it, or to choose a threshold of your own relative to
#' it.
#'
#' The threshold is computed by the same code that [add_kinematics()] uses,
#' so the two always agree. See the section "The minimum step" in
#' [add_kinematics()] for how it is estimated and when to set it yourself.
#'
#' @param data An anipoint with two or three spatial axes, grouped one
#'   trajectory per group, as it is by its declared keys. Any coordinate
#'   system works; non-Cartesian frames are converted to Cartesian for the
#'   computation.
#'
#' @return A data frame with one row per trajectory: the key columns, then
#'   - `positional_noise`: the estimated standard deviation of the tracking
#'     noise on each axis, in the frame's spatial unit. `0` when the
#'     trajectory is too short, or moves too little, to estimate it.
#'   - `min_step`: the threshold `"auto"` uses, in the frame's spatial unit:
#'     three times `positional_noise`, or half the trajectory's median step
#'     if that is smaller. Where it is less than three times
#'     `positional_noise`, the cap on the median step set it.
#'
#' @seealso [add_kinematics()], whose `min_step` takes the threshold.
#'
#' @export
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 50, n_individuals = 2, n_keypoints = 1)
#' compute_min_step(af)
#'
#' # One threshold for every trajectory, so their turning is filtered alike
#' thresholds <- compute_min_step(af)
#' add_kinematics(af, min_step = max(thresholds$min_step))
compute_min_step <- function(data) {
  anicore::ensure_is_anipoint(data)
  ensure_trajectory_grouping(data)

  if (!anicore::is_cartesian(data)) {
    data <- anispace::map_to_cartesian(data)
  }
  axes <- cartesian_axes(data)
  if (!length(axes) %in% c(2L, 3L)) {
    cli::cli_abort(
      c(
        "{.arg data} must have two or three spatial axes.",
        "i" = "{.arg min_step} applies to the direction of travel, which 1D data has none of."
      )
    )
  }
  index <- anicore::get_index(data)
  v_cols <- paste0("v_", names(axes))

  data |>
    calculate_translation() |>
    dplyr::summarise(
      auto_min_step(
        position = dplyr::pick(dplyr::all_of(unname(axes))),
        velocity = dplyr::pick(dplyr::all_of(v_cols)),
        time = .data[[index]]
      ),
      .groups = "drop"
    )
}

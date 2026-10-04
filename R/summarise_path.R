#' Summarise each trajectory as a whole
#'
#' @description
#' Measures of a path that are not a statistic of any per-row column: how
#' long it was, where it ended up relative to where it started, and how
#' directly it got there. Each needs one trajectory per group, since the
#' distance between the ends of several pooled trajectories describes
#' nothing.
#'
#' Positions are all it needs: the kinematics are computed internally, so
#' [add_kinematics()] need not be run first. Any coordinate system
#' works; non-Cartesian frames are converted to Cartesian for the
#' computation.
#'
#' For the distribution of per-row measures over each group, such as median
#' speed or the median of windowed straightness, see [summarise_aniframe()].
#'
#' @param data An anipoint, grouped one trajectory per group, as it is by its
#'   declared keys.
#'
#' @return A data frame with one row per trajectory:
#'   - `total_distance`: distance travelled, the last value of
#'     [add_kinematics()]'s `cumulative_distance`.
#'   - `total_turning`: turning of the direction of travel, summed (2D and
#'     3D), in the frame's `unit_angle`.
#'   - `net_displacement`: straight-line distance from start to end.
#'   - `straightness`: net displacement over distance travelled, from 0 to
#'     1.
#'   - `sinuosity`: corrected sinuosity index (Benhamou 2004).
#'   - `e_max`: maximum expected displacement (dimensionless).
#'
#'   The windowed measures of [add_tortuosity()] carry the window width in
#'   their names (`straightness_11`), so they never collide with these.
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. Journal of Theoretical Biology, 229(2), 209-220.
#'
#' @examples
#' traj <- anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' summarise_path(traj)
#' @export
#' @aliases summarize_path
summarise_path <- function(data) {
  anicore::ensure_is_anipoint(data)
  ensure_trajectory_grouping(data)

  if (!anicore::is_cartesian(data)) {
    data <- anispace::map_to_cartesian(data)
  }
  data <- kinematics_cartesian(data)

  axes <- cartesian_axes(data)
  position_cols <- unname(axes)
  v_cols <- paste0("v_", names(axes))

  total_turning <- if ("cumulative_turning" %in% names(data)) {
    list(
      total_turning = rlang::quo(
        dplyr::last(.data$cumulative_turning, na_rm = TRUE) -
          dplyr::first(.data$cumulative_turning, na_rm = TRUE)
      )
    )
  }

  data |>
    dplyr::summarise(
      total_distance = dplyr::last(.data$cumulative_distance, na_rm = TRUE) -
        dplyr::first(.data$cumulative_distance, na_rm = TRUE),
      !!!total_turning,
      net_displacement = vector_norm(lapply(
        dplyr::pick(dplyr::all_of(position_cols)),
        \(x) dplyr::last(x, na_rm = TRUE) - dplyr::first(x, na_rm = TRUE)
      )),

      .mean_cos_turning = mean(
        cos_turning(dplyr::pick(dplyr::all_of(v_cols))),
        na.rm = TRUE
      ),
      .n_steps = sum(!is.na(.data$cumulative_distance)) - 1L,

      .groups = "drop"
    ) |>
    dplyr::mutate(
      .mean_step_length = .data$total_distance / .data$.n_steps,

      straightness = compute_straightness(
        .data$net_displacement,
        .data$total_distance
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step_length,
        .data$.mean_cos_turning,
        method = "corrected"
      ),
      e_max = compute_emax(.data$.mean_cos_turning)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}

#' @rdname summarise_path
#' @export
summarize_path <- summarise_path

#' What `summarise_tortuosity()` returned
#'
#' @param data An anipoint.
#' @return As [summarise_path()], with `total_path_length` in place of
#'   `total_distance` and `emax` in place of `e_max`.
#' @keywords internal
summarise_path_legacy <- function(data) {
  dplyr::rename(
    summarise_path(data),
    total_path_length = "total_distance",
    emax = "e_max"
  )
}

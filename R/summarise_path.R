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
#' @param min_step `r lifecycle::badge("experimental")` The shortest step
#'   whose direction counts toward `total_turning`, as in [add_kinematics()]
#'   (default `"auto"`). `0` counts every direction, however short the step.
#'
#' @return A data frame with one row per trajectory:
#'   - `total_distance`: distance travelled, the last value of
#'     [add_kinematics()]'s `cumulative_distance`.
#'   - `total_turning`: turning of the direction of travel, summed (2D and
#'     3D), in the frame's `unit_angle`: the last value of
#'     [add_kinematics()]'s `cumulative_turning`, so steps shorter than
#'     `min_step` add none.
#'   - `net_displacement`: straight-line distance from start to end.
#'   - `straightness`: net displacement over distance travelled, from 0 to
#'     1.
#'   - `sinuosity`: corrected sinuosity index (Benhamou 2004), of the path
#'     rediscretised to a constant step length (see Details).
#'   - `e_max`: maximum expected displacement (dimensionless), from the same
#'     rediscretised path.
#'
#'   The windowed measures of [add_tortuosity()] carry the window width in
#'   their names (`straightness_11`), so they never collide with these.
#'
#' @details
#' Sinuosity is defined for a path of constant step length (Benhamou 2004),
#' so `sinuosity` and `e_max` come from the turning angles of the path
#' rediscretised to one: walking along the path, a new point is placed
#' wherever it first leaves a circle of that radius around the last one
#' (Bovet & Benhamou 1988). The step length is the trajectory's mean step
#' between rows, weighted by step length: the average step over the
#' distance travelled, which time spent still does not shorten. Tracking jitter that stays within the circle while an
#' animal is still gives no steps and no turning, where turning angles
#' between successive frames would be dominated by it. Missing positions
#' break the path into stretches that are rediscretised separately.
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. Journal of Theoretical Biology, 229(2), 209-220.
#'
#' Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals' movements
#' using a correlated random walk model. Journal of Theoretical Biology,
#' 131(4), 419-433.
#'
#' @examples
#' traj <- anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' summarise_path(traj)
#'
#' # Count every direction toward total_turning, however short the step
#' summarise_path(traj, min_step = 0)
#' @export
#' @aliases summarize_path
summarise_path <- function(data, min_step = "auto") {
  check_min_step(min_step)
  path_summary(data, min_step = min_step, rediscretise = TRUE)
}

#' @rdname summarise_path
#' @export
summarize_path <- summarise_path

#' What `summarise_tortuosity()` returned
#'
#' Every direction counts toward `total_turning`, and sinuosity and E_max
#' come from the turning between successive frames.
#'
#' @param data An anipoint.
#' @return As [summarise_path()], with `total_path_length` in place of
#'   `total_distance` and `emax` in place of `e_max`.
#' @keywords internal
summarise_path_legacy <- function(data) {
  dplyr::rename(
    path_summary(data, min_step = 0, rediscretise = FALSE),
    total_path_length = "total_distance",
    emax = "e_max"
  )
}

#' The measures of each whole trajectory
#'
#' @param data An anipoint.
#' @param min_step See [add_kinematics()].
#' @param rediscretise Whether sinuosity and E_max come from the path
#'   rediscretised to a constant step length, or, as `summarise_tortuosity()`
#'   computed them, from the turning between successive frames.
#' @param call The calling environment, for error messages.
#' @return A data frame with one row per trajectory.
#' @keywords internal
path_summary <- function(
  data,
  min_step,
  rediscretise,
  call = rlang::caller_env()
) {
  anicore::ensure_is_anipoint(data)
  ensure_trajectory_grouping(data, call = call)

  if (!anicore::is_cartesian(data)) {
    data <- anispace::map_to_cartesian(data)
  }
  data <- kinematics_cartesian(data, min_step = min_step)

  axes <- cartesian_axes(data)
  position_cols <- unname(axes)
  v_cols <- paste0("v_", names(axes))
  index <- anicore::get_index(data)

  total_turning <- if ("cumulative_turning" %in% names(data)) {
    list(
      total_turning = rlang::quo(
        dplyr::last(.data$cumulative_turning, na_rm = TRUE) -
          dplyr::first(.data$cumulative_turning, na_rm = TRUE)
      )
    )
  }

  summary <- data |>
    dplyr::summarise(
      total_distance = dplyr::last(.data$cumulative_distance, na_rm = TRUE) -
        dplyr::first(.data$cumulative_distance, na_rm = TRUE),
      !!!total_turning,
      net_displacement = vector_norm(lapply(
        dplyr::pick(dplyr::all_of(position_cols)),
        \(x) dplyr::last(x, na_rm = TRUE) - dplyr::first(x, na_rm = TRUE)
      )),
      .frame_cos_turning = mean(
        cos_turning(dplyr::pick(dplyr::all_of(v_cols))),
        na.rm = TRUE
      ),
      .n_steps = sum(!is.na(.data$cumulative_distance)) - 1L,
      .rediscretised = list(
        if (rediscretise) {
          path_sinuosity(
            dplyr::pick(dplyr::all_of(position_cols)),
            .data[[index]]
          )
        }
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      straightness = compute_straightness(
        .data$net_displacement,
        .data$total_distance
      )
    )

  if (rediscretise) {
    summary <- dplyr::mutate(
      summary,
      sinuosity = purrr::map_dbl(.data$.rediscretised, "sinuosity"),
      e_max = purrr::map_dbl(.data$.rediscretised, "e_max")
    )
  } else {
    summary <- dplyr::mutate(
      summary,
      .mean_step_length = .data$total_distance / .data$.n_steps,
      sinuosity = compute_sinuosity(
        .data$.mean_step_length,
        .data$.frame_cos_turning,
        method = "corrected"
      ),
      e_max = compute_emax(.data$.frame_cos_turning)
    )
  }
  dplyr::select(summary, -dplyr::starts_with("."))
}

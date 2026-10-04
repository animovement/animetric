#' Add tortuosity measures over sliding windows
#'
#' Computes how winding the path is (straightness, sinuosity and E_max) over
#' a window centred on each row, and returns the frame with a column for
#' each. Everything they need is computed internally and not added, so only
#' the three measures appear.
#'
#' @param data A Cartesian anipoint.
#' @param window_width Size of the sliding window, in observations (default
#'   `11L`). Should be an odd number >= 3 for symmetric centering.
#'
#' @return The input anipoint with three columns added, named with the
#'   window width, so that `window_width = 11` gives:
#'   \describe{
#'     \item{`straightness_11`}{Straightness index (D/L), from 0 to 1.}
#'     \item{`sinuosity_11`}{Corrected sinuosity index (Benhamou 2004).}
#'     \item{`e_max_11`}{Maximum expected displacement (dimensionless).}
#'   }
#'   The width in the name keeps these windowed measures apart from the
#'   whole-path `straightness`, `sinuosity` and `e_max` of [summarise_path()],
#'   and lets several window widths sit side by side.
#'
#' @details
#' Straightness is appropriate for directed/goal-oriented movement, while
#' sinuosity and E_max are appropriate for random search paths.
#'
#' Works on 1D, 2D and 3D Cartesian data, reading the axes from the frame's
#' declared variables.
#'
#' The window is centered on each timepoint. Near the ends of a trajectory,
#' where the window would run past the first or last observation, the metrics
#' are `NA`.
#'
#' **Straightness** is the distance between the positions at the ends of the
#' window over the distance travelled between them.
#'
#' **Sinuosity and E_max** come from the turning angles of the path
#' rediscretised to a constant step length, as Benhamou (2004) defines
#' sinuosity: walking along the path, a new point is placed wherever it
#' first leaves a circle of that radius around the last one (Bovet &
#' Benhamou 1988). The step length is the trajectory's mean step between
#' rows, weighted by step length: the average step over the distance
#' travelled, which time spent still does not shorten. Tracking
#' jitter that stays within the circle while an animal is still gives no
#' steps and no turning, where turning angles between successive frames
#' would be dominated by it. Each window takes the turning at the
#' rediscretised points the path reaches within it, and is `NA` when there
#' are none: where the animal moved less than a step, its path has no
#' sinuosity to measure. The path is rediscretised once per trajectory, and
#' missing positions break it into stretches that are rediscretised
#' separately.
#'
#' @references
#' Batschelet, E. (1981). Circular statistics in biology. Academic Press.
#'
#' Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals' movements
#' using a correlated random walk model. Journal of Theoretical Biology,
#' 131(4), 419-433.
#'
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal’s
#' path: straightness, sinuosity, or fractal dimension?.
#' Journal of Theoretical Biology, 229(2), 209-220.
#'
#' Cheung, A., Zhang, S., Stricker, C., & Srinivasan, M. V. (2007). Animal
#' navigation: the difficulty of moving in a straight line. Biological
#' Cybernetics, 97(1), 47-61.
#'
#' @seealso
#' * [add_kinematics()] for speed, course and the turning measures.
#' * [summarise_path()] for the same measures over each whole trajectory.
#'
#' @export
#'
#' @examples
#' data <- anicore::example_anipoint(n_obs = 30, n_individuals = 1, n_keypoints = 1)
#'
#' data |>
#'   add_tortuosity(window_width = 11)
#'
#' # Several window widths side by side
#' data |>
#'   add_tortuosity(window_width = 5) |>
#'   add_tortuosity(window_width = 11)
add_tortuosity <- function(data, window_width = 11L) {
  ensure_trajectory_grouping(data)
  window_width <- check_tortuosity_input(data, window_width)

  original_class <- class(data)
  position_cols <- unname(cartesian_axes(data))
  index <- anicore::get_index(data)
  names <- paste0(tortuosity_measures, "_", window_width)

  result <- data |>
    dplyr::mutate(
      !!names[[1]] := window_straightness(
        dplyr::pick(dplyr::all_of(position_cols)),
        window_width
      ),
      rlang::set_names(
        as.data.frame(window_sinuosity(
          dplyr::pick(dplyr::all_of(position_cols)),
          .data[[index]],
          window_width
        )),
        names[2:3]
      )
    )

  class(result) <- original_class
  result
}

#' Straightness over sliding windows
#'
#' @param position A data frame of positions, one column per axis.
#' @param window_width The window width, in rows.
#' @return Numeric vector: the distance between the positions at the ends of
#'   each row's window, over the distance travelled between them.
#' @keywords internal
window_straightness <- function(position, window_width) {
  half_w <- window_width %/% 2L
  other_half <- window_width - half_w - 1L
  travelled <- data.table::frollsum(
    step_length(position),
    n = window_width - 1L,
    algo = "fast",
    align = "center",
    na.rm = TRUE
  )
  displacement <- vector_norm(lapply(
    position,
    \(x) dplyr::lead(x, n = other_half) - dplyr::lag(x, n = half_w)
  ))
  compute_straightness(displacement, travelled)
}

# The measures add_tortuosity() adds, without their window width
tortuosity_measures <- c("straightness", "sinuosity", "e_max")

#' The columns [add_tortuosity()] has added to a frame
#'
#' Its columns are named `<measure>_<window width>`, one set per width, so
#' they are found by that pattern rather than by a fixed list.
#'
#' @param data A data frame.
#' @return The names of the windowed tortuosity columns, in frame order.
#' @keywords internal
tortuosity_columns <- function(data) {
  pattern <- paste0(
    "^(",
    paste(tortuosity_measures, collapse = "|"),
    ")_[0-9]+$"
  )
  grep(pattern, names(data), value = TRUE)
}

#' Check the input to the windowed tortuosity
#'
#' @param data An anipoint.
#' @param window_width The window width asked for.
#' @param call The calling environment, for error messages.
#' @return `window_width`, as an integer.
#' @keywords internal
check_tortuosity_input <- function(
  data,
  window_width,
  call = rlang::caller_env()
) {
  anicore::ensure_is_anipoint(data)
  if (!anicore::is_cartesian(data)) {
    cli::cli_abort("Data must be in Cartesian coordinates.", call = call)
  }
  window_width <- as.integer(window_width)
  if (window_width < 3L) {
    cli::cli_abort("{.arg window_width} must be at least 3.", call = call)
  }
  window_width
}

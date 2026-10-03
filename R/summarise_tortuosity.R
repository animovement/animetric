#' Calculate tortuosity summary statistics
#'
#' Calculate path length, displacement, and tortuosity metrics.
#'
#' @inheritParams summarise_aniframe
#'
#' @return A summarised data frame with one row per group containing:
#'
#'   - `total_path_length`: Total distance traveled
#'   - `total_turning`: Total turning of the direction of travel (2D and 3D)
#'
#'   **Tortuosity metrics:**
#'   - `net_displacement`: Straight-line distance from start to end
#'   - `straightness`: Ratio of net displacement to path length (0-1)
#'   - `sinuosity`: Corrected sinuosity index (Benhamou 2004)
#'   - `emax`: Maximum expected displacement (dimensionless)
#'
#' @references
#' Benhamou, S. (2004). How to reliably estimate the tortuosity of an animal's
#' path. Journal of Theoretical Biology, 229(2), 209-220.
#'
#' @examples
#' kin <- calculate_kinematics(
#'   anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' )
#' summarise_tortuosity(calculate_tortuosity(kin))
#' @export
#' @aliases summarize_tortuosity
summarise_tortuosity <- function(data) {
  ensure_trajectory_grouping(data)

  if (!is_aniframe_kin(data)) {
    data <- data |>
      calculate_kinematics() |>
      calculate_tortuosity()
  }

  axes <- cartesian_axes(data)
  if (length(axes) == 0L) {
    cli::cli_abort("Data must be in Cartesian coordinates.")
  }
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
      total_path_length = dplyr::last(.data$path_length, na_rm = TRUE) -
        dplyr::first(.data$path_length, na_rm = TRUE),
      !!!total_turning,
      net_displacement = vector_norm(lapply(
        dplyr::pick(dplyr::all_of(position_cols)),
        \(x) dplyr::last(x, na_rm = TRUE) - dplyr::first(x, na_rm = TRUE)
      )),

      .mean_cos_turning = mean(
        cos_turning(dplyr::pick(dplyr::all_of(v_cols))),
        na.rm = TRUE
      ),
      .n_steps = sum(!is.na(.data$path_length)) - 1L,

      .groups = "drop"
    ) |>
    dplyr::mutate(
      .mean_step_length = .data$total_path_length / .data$.n_steps,

      straightness = compute_straightness(
        .data$net_displacement,
        .data$total_path_length
      ),
      sinuosity = compute_sinuosity(
        .data$.mean_step_length,
        .data$.mean_cos_turning,
        method = "corrected"
      ),
      emax = compute_emax(.data$.mean_cos_turning)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
}

#' @rdname summarise_tortuosity
#' @export
summarize_tortuosity <- summarise_tortuosity

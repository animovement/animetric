#' Calculate kinematics from 2D movements
#'
#' @param data An aniframe
#' @return An aniframe with the original columns plus the kinematic measures:
#'   \code{distance}, \code{cumsum_distance}, \code{v_translation}, \code{a_translation},
#'   \code{direction}, \code{rotation}, \code{cumsum_rotation}, \code{v_rotation}, \code{a_rotation}.
#' @export
calculate_kinematics_2d <- function(data) {
  ensure_is_aniframe(data)
  data <- calculate_intermediate_2d(data)
  data <- calculate_translation_2d(data)
  data <- calculate_rotation_2d(data)
  data <- data |>
    dplyr::select(-c("dx", "dy")) |>
    new_aniframe_kin() |>
    new_aniframe_kin2d()
  data
}

#' Calculate kinematics from 3D movements
#'
#' @inheritParams calculate_kinematics_2d
#' @return An aniframe with the original columns plus the kinematic measures:
#'   \code{distance}, \code{cumsum_distance}, \code{v_translation}, \code{a_translation},
#' @export
calculate_kinematics_3d <- function(data) {
  ensure_is_aniframe(data)
  data <- calculate_intermediate_3d(data)
  data <- calculate_translation_3d(data)
  data <- data |>
    dplyr::select(-c("dx", "dy", "dz")) |>
    new_aniframe_kin() |>
    new_aniframe_kin3d()
  data
}

# ----------------------------------------------------------------------
# Compute raw deltas
# ----------------------------------------------------------------------
#' @keywords internal
calculate_intermediate_2d <- function(data) {
  data |>
    dplyr::mutate(
      dx = .data$x - dplyr::lag(.data$x),
      dy = .data$y - dplyr::lag(.data$y),
    )
}

#' @keywords internal
calculate_intermediate_3d <- function(data) {
  data |>
    dplyr::mutate(
      dx = .data$x - dplyr::lag(.data$x),
      dy = .data$y - dplyr::lag(.data$y),
      dz = .data$z - dplyr::lag(.data$z)
    )
}

# ----------------------------------------------------------------------
# Translational kinematics
# ----------------------------------------------------------------------
#' @keywords internal
calculate_translation_2d <- function(data) {
  data |>
    dplyr::mutate(
      d_translation = calculate_distance(.data$dx, .data$dy),
      cumsum_d_translation = cumsum_na(.data$d_translation),
      speed_translation = d_translation / (.data$time - dplyr::lag(.data$time)),
      a_translation = calculate_derivative(
          .data$speed_translation,
          .data$time,
          order = 1L
        ),
      vx_translation = calculate_derivative(
        .data$x,
        .data$time,
        order = 1L
      ),
      vy_translation = calculate_derivative(
        .data$y,
        .data$time,
        order = 1L
      ),
      ax_translation = calculate_derivative(
        .data$x,
        .data$time,
        order = 2L
      ),
      ay_translation = calculate_derivative(
        .data$y,
        .data$time,
        order = 2L
      )
    )
}

#' @keywords internal
calculate_translation_3d <- function(data) {
  data |>
    dplyr::mutate(
      d_translation = calculate_distance(.data$dx, .data$dy, .data$dz),
      cumsum_d_translation = cumsum_na(.data$d_translation),
      speed_translation = d_translation / diff(.data$time),
      v_translation = calculate_derivative(
        .data$d_translation,
        .data$time,
        order = 1L
      ),
      a_translation = calculate_derivative(
        .data$d_translation,
        .data$time,
        order = 2L
      )
    )
}

# ----------------------------------------------------------------------
# Rotational kinematics
# ----------------------------------------------------------------------
#' @keywords internal
calculate_rotation_2d <- function(data) {
  data |>
    dplyr::mutate(
      direction = calculate_direction(.data$dx, .data$dy),
      d_rotation = aniframe::diff_angle(.data$direction),
      cumsum_d_rotation = cumsum_na(.data$d_rotation),
      v_rotation = calculate_derivative(
        .data$d_rotation,
        .data$time,
        order = 1L
      ),
      a_rotation = calculate_derivative(
        .data$d_rotation,
        .data$time,
        order = 2L
      )
    )
}

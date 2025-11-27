#' Calculate kinematic measures from trajectory data
#'
#' Computes translational and rotational kinematic measures from movement data.
#' Handles data in any coordinate system by automatically converting to Cartesian
#' for calculations, then converting back to the original system.
#'
#' @param data An aniframe with position coordinates (x/y or x/y/z for Cartesian;
#'   rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for spherical)
#'   and a time column
#'
#' @return An aniframe in the same coordinate system as the input, with added
#'   kinematic measures. For 2D data, includes translational kinematics
#'   (velocity components, speed, acceleration, path length) and rotational
#'   kinematics (heading, angular velocity, angular speed, angular acceleration).
#'   For 3D data, includes translational kinematics only (rotational measures
#'   for 3D are not yet implemented).
#'
#' @details
#' The function preserves the original coordinate system by:
#' \enumerate{
#'   \item Detecting the input coordinate system from metadata
#'   \item Converting to Cartesian if necessary
#'   \item Computing kinematics in Cartesian space
#'   \item Converting back to the original coordinate system
#' }
#'
#' All kinematic calculations are performed using numerical differentiation
#' via the \code{differentiate} function. Angles are unwrapped to handle
#' discontinuities at ±π.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 2D Cartesian data
#' traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
#'   as_aniframe()
#' kinematics_2d <- calculate_kinematics(traj_2d)
#'
#' # Polar data (automatically converted and converted back)
#' traj_polar <- aniframe::map_to_polar(traj_2d)
#' kinematics_polar <- calculate_kinematics(traj_polar)
#' }
calculate_kinematics <- function(data) {
  ensure_is_aniframe(data)

  # Convert to Cartesian if needed
  original_system <- aniframe::get_metadata(data)$coordinate_system
  if (!aniframe::is_cartesian(data)) {
    data <- aniframe::map_to_cartesian(data)
  }

  # Calculate kinematics
  if (aniframe::is_cartesian_2d(data)) {
    data <- calculate_kinematics_2d(data)
  } else if (aniframe::is_cartesian_3d(data)) {
    data <- calculate_kinematics_3d(data)
  }

  # Convert back if needed
  if (as.character(original_system) == "polar") {
    data <- aniframe::map_to_polar(data)
  } else if (as.character(original_system) == "cylindrical") {
    data <- aniframe::map_to_cylindrical(data)
  } else if (as.character(original_system) == "spherical") {
    data <- aniframe::map_to_spherical(data)
  }

  data
}

#' @rdname calculate_kinematics
#' @export
calculate_kinematics_2d <- function(data) {
  ensure_is_aniframe(data)
  data <- calculate_translation_2d(data)
  data <- calculate_rotation_2d(data)
  data |>
    new_aniframe_kin() |>
    new_aniframe_kin2d()
}

#' @rdname calculate_kinematics
#' @export
calculate_kinematics_3d <- function(data) {
  ensure_is_aniframe(data)
  data <- calculate_translation_3d(data)
  data |>
    new_aniframe_kin() |>
    new_aniframe_kin3d()
}

#' Calculate translational kinematics in 2D
#'
#' @param data An aniframe with x, y, and time columns
#' @return The aniframe with added translational kinematic columns
#' @keywords internal
calculate_translation_2d <- function(data) {
  data |>
    dplyr::mutate(
      v_x = differentiate(.data$x, .data$time, order = 1),
      v_y = differentiate(.data$y, .data$time, order = 1),
      a_x = differentiate(.data$x, .data$time, order = 2),
      a_y = differentiate(.data$y, .data$time, order = 2),
      speed = sqrt(.data$v_x^2 + .data$v_y^2),
      acceleration = differentiate(.data$speed, .data$time, order = 1),
      path_length = cumsum_na(sqrt(
        (.data$x - dplyr::lag(.data$x))^2 + (.data$y - dplyr::lag(.data$y))^2
      ))
    ) |>
    dplyr::relocate("speed", .before = "v_x") |>
    dplyr::relocate("acceleration", .before = "v_x") |>
    dplyr::relocate("path_length", .before = "v_x")
}

#' Calculate translational kinematics in 3D
#'
#' @param data An aniframe with x, y, z, and time columns
#' @return The aniframe with added translational kinematic columns
#' @keywords internal
calculate_translation_3d <- function(data) {
  data |>
    dplyr::mutate(
      v_x = differentiate(.data$x, .data$time, order = 1),
      v_y = differentiate(.data$y, .data$time, order = 1),
      v_z = differentiate(.data$z, .data$time, order = 1),
      a_x = differentiate(.data$x, .data$time, order = 2),
      a_y = differentiate(.data$y, .data$time, order = 2),
      a_z = differentiate(.data$z, .data$time, order = 2),
      speed = sqrt(.data$v_x^2 + .data$v_y^2 + .data$v_z^2),
      acceleration = differentiate(.data$speed, .data$time, order = 1),
      path_length = cumsum_na(sqrt(
        (.data$x - dplyr::lag(.data$x))^2 +
          (.data$y - dplyr::lag(.data$y))^2 +
          (.data$z - dplyr::lag(.data$z))^2
      ))
    ) |>
    dplyr::relocate("speed", .before = "v_x") |>
    dplyr::relocate("acceleration", .before = "v_x") |>
    dplyr::relocate("path_length", .before = "v_x")
}

#' Calculate rotational kinematics in 2D
#'
#' Computes heading angles and angular kinematics based on the velocity vector.
#' Heading is calculated as atan2(v_y, v_x).
#'
#' @param data An aniframe with v_x, v_y, and time columns
#' @return The aniframe with added rotational kinematic columns
#' @keywords internal
calculate_rotation_2d <- function(data) {
  data |>
    dplyr::mutate(
      heading = atan2(.data$v_y, .data$v_x),
      heading = dplyr::if_else(.data$heading == pi, 0, .data$heading),
      heading_unwrapped = unwrap_angle(.data$heading),
      angular_path_length = cumsum_na(abs(diff(c(
        0,
        .data$heading_unwrapped
      )))) -
        dplyr::first(.data$heading_unwrapped),
      angular_velocity = differentiate(
        .data$heading_unwrapped,
        .data$time,
        order = 1
      ),
      angular_speed = abs(.data$angular_velocity),
      angular_acceleration = differentiate(
        .data$heading_unwrapped,
        .data$time,
        order = 2
      )
    ) |>
    dplyr::relocate("angular_speed", .before = "angular_path_length") |>
    dplyr::relocate("angular_velocity", .before = "angular_path_length") |>
    dplyr::relocate("angular_acceleration", .before = "angular_path_length")
}

#' Calculate rotational kinematics in 3D
#'
#' Computes 3D orientation angles and angular kinematics based on the velocity vector.
#' Uses spherical coordinates: azimuth (horizontal angle) and elevation (vertical angle).
#'
#' @param data An aniframe with v_x, v_y, v_z, and time columns
#' @return The aniframe with added rotational kinematic columns
#' @keywords internal
calculate_rotation_3d <- function(data) {
  data |>
    dplyr::mutate(
      # Azimuth: angle in xy-plane (like heading in 2D)
      azimuth = atan2(.data$v_y, .data$v_x),
      azimuth = dplyr::if_else(.data$azimuth == pi, 0, .data$azimuth),
      azimuth_unwrapped = unwrap_angle(.data$azimuth),

      # Elevation: angle from xy-plane
      elevation = atan2(.data$v_z, sqrt(.data$v_x^2 + .data$v_y^2)),
      elevation_unwrapped = unwrap_angle(.data$elevation),

      # Angular velocities for each axis
      angular_velocity_azimuth = differentiate(
        .data$azimuth_unwrapped,
        .data$time,
        order = 1
      ),
      angular_velocity_elevation = differentiate(
        .data$elevation_unwrapped,
        .data$time,
        order = 1
      ),

      # Total angular speed (magnitude)
      angular_speed = sqrt(
        .data$angular_velocity_azimuth^2 + .data$angular_velocity_elevation^2
      ),

      # Angular path lengths
      angular_path_length_azimuth = cumsum_na(abs(diff(c(
        0,
        .data$azimuth_unwrapped
      )))) -
        dplyr::first(.data$azimuth_unwrapped),
      angular_path_length_elevation = cumsum_na(abs(diff(c(
        0,
        .data$elevation_unwrapped
      )))) -
        dplyr::first(.data$elevation_unwrapped),

      # Angular accelerations
      angular_acceleration_azimuth = differentiate(
        .data$azimuth_unwrapped,
        .data$time,
        order = 2
      ),
      angular_acceleration_elevation = differentiate(
        .data$elevation_unwrapped,
        .data$time,
        order = 2
      )
    ) |>
    dplyr::relocate("angular_speed", .before = "angular_velocity_azimuth")
}

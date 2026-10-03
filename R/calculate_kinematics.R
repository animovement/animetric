#' Calculate kinematic measures from trajectory data
#'
#' Computes translational and rotational kinematic measures from movement data.
#' Handles data in any coordinate system by automatically converting to Cartesian
#' for calculations, then converting back to the original system.
#'
#' @param data An anipoint with position coordinates (x, x/y or x/y/z for
#'   Cartesian; rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
#'   spherical) and an index. The axes and the index are read from the frame's
#'   declared variables, so the columns can have any name.
#'
#' @return An anipoint in the same coordinate system as the input, with added
#'   kinematic measures. Translational kinematics (velocity and acceleration
#'   components, speed, acceleration, path length) are computed for 1D, 2D and
#'   3D data, with components named by axis role (`v_x`, `v_y`, ...) whatever
#'   the input columns are called. For 2D data, rotational kinematics are added
#'   too (heading, angular velocity, angular speed, angular acceleration).
#'   Heading is the direction of travel, so it is `NA` where speed is zero.
#'   Rotational measures for 1D and 3D are not yet implemented.
#'
#'   Angular measures are in the frame's declared `unit_angle`, radians or
#'   degrees; angular velocity and acceleration are per unit of the index.
#'   Signed angles (heading, angular velocity) follow the frame's own
#'   convention: heading counts from the `x` axis toward the `y` axis, which
#'   is counter-clockwise when [anicore::get_angle_direction()] says so (`y`
#'   pointing up, as aniread leaves image data) and clockwise in a frame whose
#'   `y` points down. To change convention, change the coordinates, for
#'   example with [anicore::reflect_axis()], and the angles follow.
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
#' # 2D Cartesian data
#' traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
#'   anicore::as_anipoint()
#' kinematics_2d <- calculate_kinematics(traj_2d)
#'
#' # Polar data (automatically converted and converted back)
#' traj_polar <- anispace::map_to_polar(traj_2d)
#' kinematics_polar <- calculate_kinematics(traj_polar)
calculate_kinematics <- function(data) {
  ensure_trajectory_grouping(data)
  anicore::ensure_is_anipoint(data)

  # Convert to Cartesian if needed
  original_system <- anicore::get_metadata(data, "coordinate_system")
  if (!anicore::is_cartesian(data)) {
    data <- anispace::map_to_cartesian(data)
  }

  data <- new_aniframe_kin(add_kinematics(data))

  # Convert back if needed
  if (as.character(original_system) == "polar") {
    data <- anispace::map_to_polar(data)
  } else if (as.character(original_system) == "cylindrical") {
    data <- anispace::map_to_cylindrical(data)
  } else if (as.character(original_system) == "spherical") {
    data <- anispace::map_to_spherical(data)
  }

  data
}

#' Add translational, and where defined rotational, kinematics
#'
#' @param data A Cartesian anipoint.
#' @return The anipoint with added kinematic columns.
#' @keywords internal
add_kinematics <- function(data) {
  data <- calculate_translation(data)
  if (anicore::is_cartesian_2d(data)) {
    data <- calculate_rotation_2d(data)
  }
  data
}

#' Calculate translational kinematics
#'
#' Works on any number of Cartesian axes. Velocity and acceleration components
#' are named by axis role (`v_x`, `a_x`, ...), speed and step length are
#' Euclidean norms over the axes.
#'
#' @param data A Cartesian anipoint.
#' @return The anipoint with added translational kinematic columns
#' @keywords internal
calculate_translation <- function(data) {
  axes <- cartesian_axes(data)
  index <- anicore::get_index(data)
  v_cols <- paste0("v_", names(axes))
  a_cols <- paste0("a_", names(axes))

  derivative <- function(col, order) {
    rlang::quo(differentiate(.data[[!!col]], .data[[!!index]], order = !!order))
  }
  derivatives <- c(
    rlang::set_names(purrr::map(axes, derivative, order = 1L), v_cols),
    rlang::set_names(purrr::map(axes, derivative, order = 2L), a_cols)
  )

  data |>
    dplyr::mutate(!!!derivatives) |>
    dplyr::mutate(
      speed = vector_norm(dplyr::pick(dplyr::all_of(v_cols))),
      acceleration = differentiate(
        .data$speed,
        .data[[index]],
        order = 1
      ),
      path_length = cumsum_na(
        step_length(dplyr::pick(dplyr::all_of(unname(axes))))
      )
    ) |>
    dplyr::relocate("speed", .before = dplyr::all_of(v_cols[1])) |>
    dplyr::relocate("acceleration", .before = dplyr::all_of(v_cols[1])) |>
    dplyr::relocate("path_length", .before = dplyr::all_of(v_cols[1]))
}

#' Calculate rotational kinematics in 2D
#'
#' Computes heading angles and angular kinematics based on the velocity vector.
#' Heading is calculated as atan2(v_y, v_x), and is `NA` where speed is zero.
#' The angles are computed in radians and returned in the frame's
#' `unit_angle`.
#'
#' @param data An anipoint with v_x, v_y and speed columns, and an index
#' @return The anipoint with added rotational kinematic columns
#' @keywords internal
calculate_rotation_2d <- function(data) {
  index <- anicore::get_index(data)
  unit <- angle_unit(data)
  angular_cols <- c(
    "heading",
    "heading_unwrapped",
    "angular_path_length",
    "angular_velocity",
    "angular_speed",
    "angular_acceleration"
  )

  data |>
    dplyr::mutate(
      # Direction of travel is undefined when the animal is not moving
      heading = dplyr::if_else(
        .data$speed == 0,
        NA_real_,
        atan2(.data$v_y, .data$v_x)
      ),
      heading_unwrapped = anicore::unwrap_angle(.data$heading),
      angular_path_length = cumsum_turning(.data$heading),
      angular_velocity = differentiate(
        .data$heading_unwrapped,
        .data[[index]],
        order = 1
      ),
      angular_speed = abs(.data$angular_velocity),
      angular_acceleration = differentiate(
        .data$heading_unwrapped,
        .data[[index]],
        order = 2
      )
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(angular_cols),
      \(x) rad_to_unit(x, unit)
    )) |>
    dplyr::relocate("angular_speed", .before = "angular_path_length") |>
    dplyr::relocate("angular_velocity", .before = "angular_path_length") |>
    dplyr::relocate("angular_acceleration", .before = "angular_path_length")
}

#' Calculate rotational kinematics in 3D
#'
#' Computes 3D orientation angles and angular kinematics based on the velocity vector.
#' Uses spherical coordinates: azimuth (horizontal angle) and elevation (vertical angle).
#'
#' @param data An anipoint with v_x, v_y, v_z, and time columns
#' @return The anipoint with added rotational kinematic columns
#' @keywords internal
calculate_rotation_3d <- function(data) {
  # data |>
  #   dplyr::mutate(
  #     # Azimuth: angle in xy-plane (like heading in 2D)
  #     azimuth = atan2(.data$v_y, .data$v_x),
  #     azimuth = dplyr::if_else(.data$azimuth == pi, 0, .data$azimuth),
  #     azimuth_unwrapped = anicore::unwrap_angle(.data$azimuth),
  #     # Elevation: angle from xy-plane
  #     elevation = atan2(.data$v_z, sqrt(.data$v_x^2 + .data$v_y^2)),
  #     elevation_unwrapped = anicore::unwrap_angle(.data$elevation),
  #     # Angular velocities for each axis
  #     angular_velocity_azimuth = differentiate(
  #       .data$azimuth_unwrapped,
  #       .data$time,
  #       order = 1
  #     ),
  #     angular_velocity_elevation = differentiate(
  #       .data$elevation_unwrapped,
  #       .data$time,
  #       order = 1
  #     ),
  #     # Total angular speed (magnitude)
  #     angular_speed = sqrt(
  #       .data$angular_velocity_azimuth^2 + .data$angular_velocity_elevation^2
  #     ),
  #     # Angular path lengths
  #     angular_path_length_azimuth = cumsum_na(abs(diff(c(
  #       0,
  #       .data$azimuth_unwrapped
  #     )))) -
  #       dplyr::first(.data$azimuth_unwrapped),
  #     angular_path_length_elevation = cumsum_na(abs(diff(c(
  #       0,
  #       .data$elevation_unwrapped
  #     )))) -
  #       dplyr::first(.data$elevation_unwrapped),
  #     # Angular accelerations
  #     angular_acceleration_azimuth = differentiate(
  #       .data$azimuth_unwrapped,
  #       .data$time,
  #       order = 2
  #     ),
  #     angular_acceleration_elevation = differentiate(
  #       .data$elevation_unwrapped,
  #       .data$time,
  #       order = 2
  #     )
  #   ) |>
  #   dplyr::relocate("angular_speed", .before = "angular_velocity_azimuth")
}

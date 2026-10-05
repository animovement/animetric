#' Add kinematic measures to trajectory data
#'
#' Computes translational and rotational kinematic measures from movement
#' data, and returns the frame with a column for each. Handles data in any
#' coordinate system by automatically converting to Cartesian for the
#' calculations, then converting back to the original system.
#'
#' @param data An anipoint with position coordinates (x, x/y or x/y/z for
#'   Cartesian; rho/phi for polar; rho/phi/z for cylindrical; rho/phi/theta for
#'   spherical) and an index. The axes and the index are read from the frame's
#'   declared variables, so the columns can have any name.
#' @param vertical `r lifecycle::badge("experimental")` For 3D data, the
#'   axis that points up in the world, against gravity: one of `"x"`, `"y"`
#'   or `"z"`, or with a minus sign (`"-y"`) when that axis points down. It
#'   defines the horizontal plane that course is measured in. The frame's
#'   `axis_directions` cannot supply it, since they are relative to the
#'   camera: in a recording filmed from above, the axis pointing at the
#'   camera is the vertical one. A metadata field for it is proposed in
#'   animovement/anicore#172. `NULL` (the default) gives only the measures
#'   that need no vertical. Ignored for 1D and 2D data, where course is
#'   measured in the plane of the data. Experimental until that proposal is
#'   settled: the argument may come to default to the declared vertical, and
#'   to mean something for 2D side views.
#' @param min_step `r lifecycle::badge("experimental")` The shortest step,
#'   in the frame's spatial unit, whose direction counts. Where a point moves
#'   less than this per sample, its direction of travel is set by tracking
#'   noise rather than by where the animal is going, so the direction is
#'   treated as undefined: `course` is `NA` there, and the row adds no
#'   turning. One of:
#'   \describe{
#'     \item{`"auto"` (the default)}{Three times the tracking noise,
#'       estimated separately for each trajectory, and at most half its
#'       median step. See Details, and [compute_min_step()] for the value
#'       it chooses.}
#'     \item{a number}{A threshold of your own, such as the tracking
#'       precision in the frame's spatial unit.}
#'     \item{`0`}{Every direction counts, however short the step, as
#'       [calculate_kinematics()] did.}
#'   }
#'   It affects only the direction measures (`course` and the turning
#'   measures), not `speed` or `cumulative_distance`.
#'
#' @return An anipoint in the same coordinate system as the input, with added
#'   kinematic measures. Translational measures are computed for 1D, 2D and
#'   3D data:
#'   \describe{
#'     \item{`v_x`, `v_y`, `v_z`}{Velocity components, named by axis role
#'       whatever the input columns are called.}
#'     \item{`a_x`, `a_y`, `a_z`}{Acceleration components, the second
#'       derivatives of the positions.}
#'     \item{`speed`}{The size of the velocity.}
#'     \item{`acceleration`}{The signed rate of change of `speed`: positive
#'       when speeding up, negative when slowing down. This is the tangential
#'       acceleration, along the path. It is not the size of the acceleration
#'       vector (`a_x`, `a_y`), which also includes the part that turns the
#'       path: an animal rounding a corner at constant speed has an
#'       `acceleration` of 0 but non-zero `a_x` and `a_y`.}
#'     \item{`cumulative_distance`}{Distance travelled since the first row:
#'       the straight-line steps between successive positions, summed. A step
#'       to or from a missing position adds nothing.}
#'   }
#'   For 2D and 3D data, measures of how the direction of travel changes are
#'   added too:
#'   \describe{
#'     \item{`turning_speed`}{How fast the direction of travel turns, in any
#'       direction: curvature times speed. In 2D it is the size of the turning
#'       rate. In 3D it also counts climbing and diving, and is the angle
#'       between the velocities either side of each row, over the time between
#'       them.}
#'     \item{`cumulative_turning`}{Turning accumulated since the first row:
#'       the angle between successive velocities, summed. A turn made while
#'       stationary, or while moving less than `min_step`, is counted when
#'       the animal moves off, as the angle between the directions before
#'       and after.}
#'   }
#'   In 2D, and in 3D when `vertical` is given, the measures that need a
#'   direction to count from:
#'   \describe{
#'     \item{`course`}{Direction of travel in the horizontal plane (in 2D, the
#'       plane of the data): `atan2(v_y, v_x)` in 2D. It is `NA` where there is
#'       no horizontal movement, or less than `min_step` per sample, since
#'       the direction is then undefined.
#'       `course_unwrapped` is the same, without jumps at +/-pi.}
#'     \item{`course_elevation`}{3D only: the angle of travel above the
#'       horizontal plane, from -pi/2 (straight down) to pi/2 (straight up).}
#'     \item{`turning_rate`}{Signed rate of change of the course, the
#'       derivative of `course_unwrapped`. In 3D it counts horizontal turning
#'       only.}
#'     \item{`turning_acceleration`}{Rate of change of the turning rate.}
#'   }
#'   These describe the path, not the body: course is where the animal is
#'   going, not where it is facing, and the two differ for any animal that
#'   does not move nose-first. The names heading and angular velocity are
#'   kept for body orientation. 1D data gets translational measures only.
#'
#'   Angular measures are in the frame's declared `unit_angle`, radians or
#'   degrees; turning rate and acceleration are per unit of the index.
#'   Signed angles (course, turning rate) follow the frame's own
#'   convention: course counts from the `x` axis toward the `y` axis, which
#'   is counter-clockwise when [anicore::get_angle_direction()] says so (`y`
#'   pointing up, as aniread leaves image data) and clockwise in a frame whose
#'   `y` points down. To change convention, change the coordinates, for
#'   example with [anicore::reflect_axis()], and the angles follow. In 3D,
#'   course counts about `vertical` by the right-hand rule, from the next
#'   axis in the cycle x, y, z: from `x` toward `y` when `z` is vertical, from
#'   `z` toward `x` when `y` is, from `y` toward `z` when `x` is.
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
#' via the \code{differentiate} function. Course is unwrapped before it is
#' differentiated, so the turning rate has no jump at +/-pi. The turning
#' speed in 3D needs no angle to unwrap, and has no singularity when travel
#' is vertical.
#'
#' ## The minimum step
#'
#' When a point barely moves, the direction between successive positions is
#' set by sub-pixel tracking noise, and swings at random from frame to
#' frame. Every swing is turning to the measures above, so a resting animal
#' accumulates turning as fast as a running one. `min_step` sets the step
#' below which the direction is not trusted.
#'
#' The step of a row is the distance its velocity implies moving per sample:
#' `speed` times the sampling interval, which for the central differences
#' used here is half the distance between the positions either side. Course
#' is measured against the horizontal part of the step alone.
#'
#' With `min_step = "auto"`, the threshold is three times the positional
#' noise, \eqn{\sigma}, estimated for each trajectory from its second
#' differences \eqn{p_{i+1} - 2 p_i + p_{i-1}}. For white noise of standard
#' deviation \eqn{\sigma} on each axis these have standard deviation
#' \eqn{\sqrt{6}\sigma}. Only their component along the direction of travel
#' is used, which a path turning at constant speed leaves at zero, and their
#' spread is measured with [stats::mad()], which a minority of large values
#' barely moves. A point standing still under white noise makes steps longer
#' than \eqn{3\sigma} with a probability of about 0.01% (0.04% in 3D), so
#' the threshold removes the directions of a stationary point while keeping
#' those of real movement.
#'
#' The threshold is also at most half the trajectory's median step (among
#' steps that have a direction), so most steps always keep theirs. This
#' matters when the sampling is coarse for the movement: a random walk
#' recorded step by step has second differences as large as its steps, and
#' the estimate would take the movement itself for noise.
#'
#' The estimate assumes the sampling is fast compared with changes in speed,
#' as it is for video tracking. Two cases call for setting `min_step`
#' yourself:
#' \itemize{
#'   \item **Smoothed positions.** Smoothing turns noise into slow wander,
#'     which second differences see less of, so the estimate is smaller than
#'     the jitter left in the data, and the threshold removes only part of it.
#'   \item **Coarse sampling**, such as GPS fixes minutes apart, where speed
#'     changes a lot between samples. The estimate then includes real
#'     movement, so the threshold may be too large; use the positional
#'     precision instead, or `0`.
#' }
#' The threshold is not stored in the result. [compute_min_step()] returns
#' the one `"auto"` chooses for each trajectory, with the noise estimate,
#' and a number given as `min_step` is used as it is.
#'
#' @seealso [add_tortuosity()] for windowed measures of how winding the path
#'   is, and [summarise_path()] for measures of each whole trajectory.
#'
#' @export
#'
#' @examples
#' # 2D Cartesian data
#' traj_2d <- data.frame(time = 0:10, x = rnorm(11), y = rnorm(11)) |>
#'   anicore::as_anipoint()
#' kinematics_2d <- add_kinematics(traj_2d)
#'
#' # Polar data (automatically converted and converted back)
#' traj_polar <- anispace::map_to_polar(traj_2d)
#' kinematics_polar <- add_kinematics(traj_polar)
#'
#' # 3D data with z pointing up: course and elevation of travel
#' traj_3d <- data.frame(
#'   time = 0:10,
#'   x = cos(0:10 / 2),
#'   y = sin(0:10 / 2),
#'   z = 0:10 / 10
#' ) |>
#'   anicore::as_anipoint()
#' kinematics_3d <- add_kinematics(traj_3d, vertical = "z")
#'
#' # A threshold of your own, in the frame's spatial unit, or 0 to count
#' # every direction however short the step
#' kinematics_2d <- add_kinematics(traj_2d, min_step = 0.1)
add_kinematics <- function(data, vertical = NULL, min_step = "auto") {
  ensure_trajectory_grouping(data)
  anicore::ensure_is_anipoint(data)
  check_vertical(vertical)
  check_min_step(min_step)

  # Convert to Cartesian if needed
  original_system <- anicore::get_metadata(data, "coordinate_system")
  if (!anicore::is_cartesian(data)) {
    data <- anispace::map_to_cartesian(data)
  }

  data <- kinematics_cartesian(data, vertical = vertical, min_step = min_step)

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
#' @param vertical,min_step See [add_kinematics()].
#' @return The anipoint with added kinematic columns.
#' @keywords internal
kinematics_cartesian <- function(data, vertical = NULL, min_step = 0) {
  data <- calculate_translation(data)
  if (length(cartesian_axes(data)) >= 2L) {
    data <- calculate_rotation(data, vertical = vertical, min_step = min_step)
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
      cumulative_distance = cumsum_na(
        step_length(dplyr::pick(dplyr::all_of(unname(axes))))
      )
    ) |>
    dplyr::relocate("speed", .before = dplyr::all_of(v_cols[1])) |>
    dplyr::relocate("acceleration", .before = dplyr::all_of(v_cols[1])) |>
    dplyr::relocate("cumulative_distance", .before = dplyr::all_of(v_cols[1]))
}

#' Calculate how the direction of travel changes
#'
#' Computes the turning measures of the path from the velocity vectors, for 2D
#' and 3D data. A 2D vector is treated as lying
#' in the horizontal plane of a 3D one, so both share one computation. The
#' angles are computed in radians and returned in the frame's `unit_angle`.
#'
#' @param data A 2D or 3D Cartesian anipoint with velocity (`v_*`) columns,
#'   and an index.
#' @param vertical,min_step See [add_kinematics()].
#' @return The anipoint with added rotational kinematic columns.
#' @keywords internal
calculate_rotation <- function(data, vertical = NULL, min_step = 0) {
  axes <- cartesian_axes(data)
  roles <- names(axes)
  index <- anicore::get_index(data)
  unit <- anicore::get_metadata(data, "unit_angle")
  up <- vertical_vector(length(axes), vertical)

  data |>
    dplyr::mutate(path_rotation(
      velocity = dplyr::pick(dplyr::all_of(paste0("v_", roles))),
      time = .data[[index]],
      up = up,
      min_step = resolve_min_step(
        min_step,
        position = dplyr::pick(dplyr::all_of(unname(axes))),
        velocity = dplyr::pick(dplyr::all_of(paste0("v_", roles))),
        time = .data[[index]]
      )
    )) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "course",
        "course_unwrapped",
        "course_elevation",
        "turning_speed",
        "turning_rate",
        "turning_acceleration",
        "cumulative_turning"
      )),
      \(x) anicore::angle_from_rad(x, unit)
    ))
}

#' The turning measures of one trajectory
#'
#' Every rate is the derivative of an angle, not a formula in velocity and
#' acceleration: `|v x a| / |v|^2` measures the sine of a turn, which falls
#' back toward 0 as the turn nears pi, so a sharp reversal in jittery tracking
#' would read as no turn at all.
#'
#' @param velocity A data frame of velocity components, one column per axis.
#' @param time The index.
#' @param up The unit vertical, as from [vertical_vector()], or `NULL` for
#'   only the measures that need none.
#' @param min_step The shortest step whose direction counts, a number.
#' @return A data frame of turning measures, in radians.
#' @keywords internal
path_rotation <- function(velocity, time, up, min_step = 0) {
  v <- as_3d(velocity)
  interval <- sampling_interval(time)
  moving <- has_direction(sqrt(rowSums(v^2)) * interval, min_step)
  # A direction too short to trust counts as no direction at all
  v[!moving, ] <- NA_real_

  out <- list()
  if (!is.null(up)) {
    basis <- horizontal_basis(up)
    along <- drop(v %*% basis$first)
    across <- drop(v %*% basis$second)
    horizontal_sq <- along^2 + across^2

    # Course is undefined where there is no horizontal movement, or too
    # little to give a direction
    course <- ifelse(
      has_direction(sqrt(horizontal_sq) * interval, min_step),
      atan2(across, along),
      NA_real_
    )
    course_unwrapped <- anicore::unwrap_angle(course)
    out$course <- course
    out$course_unwrapped <- course_unwrapped
    if (ncol(velocity) == 3L) {
      out$course_elevation <- ifelse(
        moving,
        atan2(drop(v %*% up), sqrt(horizontal_sq)),
        NA_real_
      )
    }
  }

  # In 2D the turning speed is the size of the turning rate; in 3D it also
  # counts climbing and diving, so it comes from the velocities themselves
  out$turning_speed <- if (ncol(velocity) == 2L) {
    abs(differentiate(course_unwrapped, time))
  } else {
    direction_change_rate(v, time)
  }

  if (!is.null(up)) {
    out$turning_rate <- differentiate(course_unwrapped, time)
    out$turning_acceleration <- differentiate(course_unwrapped, time, order = 2)
  }

  out$cumulative_turning <- cumulative_turning(v, moving)
  as.data.frame(out)
}

#' How fast the direction of travel changes, in any number of dimensions
#'
#' The angle between the velocities either side of each row, over the time
#' between them: a central difference of the direction, with one-sided ones at
#' the ends. It needs no reference direction, so it has no wrap at +/-pi and
#' no singularity when travel is vertical.
#'
#' @param v Numeric matrix of velocities, one row per observation.
#' @param time The index.
#' @return Numeric vector, in radians per unit of `time`. `NA` where either
#'   neighbouring velocity is zero or missing.
#' @keywords internal
direction_change_rate <- function(v, time) {
  n <- nrow(v)
  if (n < 2L) {
    return(rep(NA_real_, n))
  }
  before <- pmax(seq_len(n) - 1L, 1L)
  after <- pmin(seq_len(n) + 1L, n)
  anicore::angle_between(
    v[before, , drop = FALSE],
    v[after, , drop = FALSE]
  ) /
    (time[after] - time[before])
}

#' Check a `vertical` argument
#'
#' @param vertical See [add_kinematics()].
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_vertical <- function(vertical, call = rlang::caller_env()) {
  allowed <- c("x", "y", "z", "-x", "-y", "-z")
  if (
    !is.null(vertical) &&
      (!rlang::is_string(vertical) || !vertical %in% allowed)
  ) {
    cli::cli_abort(
      "{.arg vertical} must be {.code NULL} or one of {.or {.val {allowed}}}.",
      call = call
    )
  }
  invisible(TRUE)
}

#' The unit vertical of a frame
#'
#' @param n_axes How many Cartesian axes the frame has.
#' @param vertical See [add_kinematics()], already checked.
#' @return A length-3 unit vector, or `NULL` when a 3D frame has no
#'   `vertical`. In 2D it is the normal of the data's plane.
#' @keywords internal
vertical_vector <- function(n_axes, vertical) {
  if (n_axes == 2L) {
    return(c(0, 0, 1))
  }
  if (is.null(vertical)) {
    return(NULL)
  }
  up <- as.numeric(c("x", "y", "z") == sub("^-", "", vertical))
  if (startsWith(vertical, "-")) -up else up
}

#' The horizontal axes course is measured in
#'
#' Course counts about the vertical by the right-hand rule, from the axis
#' after it in the cycle x, y, z.
#'
#' @param up A unit vertical along one of the axes.
#' @return A list of two unit vectors, `first` (course 0) and `second`
#'   (course pi/2).
#' @keywords internal
horizontal_basis <- function(up) {
  axis <- which(up != 0)
  first <- as.numeric(seq_len(3L) == axis %% 3L + 1L)
  second <- drop(cross_rows(rbind(up), rbind(first)))
  list(first = first, second = second)
}

#' The time each row's velocity spans, per sample
#'
#' Velocity is a central difference, so the distance it implies moving per
#' sample is speed times half the time between the rows either side: half
#' the distance between those two positions. The ends use one-sided
#' differences, as [differentiate()] does.
#'
#' @param time The index.
#' @return Numeric vector, in units of the index.
#' @keywords internal
sampling_interval <- function(time) {
  differentiate(time)
}

#' Is a step long enough to give a direction?
#'
#' @param step Distance moved per sample.
#' @param min_step The shortest step that gives a direction.
#' @return Logical vector: `FALSE` where `step` is missing, zero, or shorter
#'   than `min_step`.
#' @keywords internal
has_direction <- function(step, min_step) {
  !is.na(step) & step > 0 & step >= min_step
}

#' Check a `min_step` argument
#'
#' @param min_step See [add_kinematics()].
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_min_step <- function(min_step, call = rlang::caller_env()) {
  valid <- identical(min_step, "auto") ||
    (is.numeric(min_step) &&
      length(min_step) == 1L &&
      !is.na(min_step) &&
      min_step >= 0)
  if (!valid) {
    cli::cli_abort(
      "{.arg min_step} must be {.val auto} or a single number of at least 0.",
      call = call
    )
  }
  invisible(TRUE)
}

#' The minimum step of one trajectory
#'
#' @param min_step See [add_kinematics()], already checked.
#' @param position A data frame of positions, one column per axis.
#' @param velocity A data frame of velocities, one column per axis.
#' @param time The index.
#' @return A number: `min_step` itself, or for `"auto"` the threshold from
#'   [auto_min_step()].
#' @keywords internal
resolve_min_step <- function(min_step, position, velocity, time) {
  if (!identical(min_step, "auto")) {
    return(min_step)
  }
  auto_min_step(position, velocity, time)$min_step
}

#' The minimum step `"auto"` chooses for one trajectory
#'
#' The one place the automatic threshold is computed, for both
#' [add_kinematics()] and [compute_min_step()].
#'
#' @param position A data frame of positions, one column per axis.
#' @param velocity A data frame of velocities, one column per axis.
#' @param time The index.
#' @return A one-row data frame: `positional_noise`, from
#'   [positional_noise()], and `min_step`, three times that noise, at most
#'   half the median step.
#' @keywords internal
auto_min_step <- function(position, velocity, time) {
  v <- as.matrix(as.data.frame(velocity))
  step <- sqrt(rowSums(v^2)) * sampling_interval(time)
  cap <- stats::median(step[!is.na(step) & step > 0]) / 2
  noise <- positional_noise(position, velocity)
  data.frame(
    positional_noise = noise,
    min_step = min(3 * noise, cap, na.rm = TRUE)
  )
}

#' Positional noise of a trajectory
#'
#' The robust standard deviation of the second differences of position along
#' the direction of travel, over `sqrt(6)`: for white noise of standard
#' deviation `sigma` on each axis, a second difference has standard deviation
#' `sqrt(6) * sigma` in any direction. Along the direction of travel, a path
#' turning at constant speed contributes nothing.
#'
#' @param position A data frame of positions, one column per axis.
#' @param velocity A data frame of velocities, one column per axis.
#' @return A number, in the unit of the positions. `0` when there are too
#'   few rows, or too little movement, to estimate it.
#' @keywords internal
positional_noise <- function(position, velocity) {
  p <- as.matrix(as.data.frame(position))
  v <- as.matrix(as.data.frame(velocity))
  n <- nrow(p)
  if (n < 3L) {
    return(0)
  }
  middle <- 2:(n - 1L)
  second <- p[middle + 1L, , drop = FALSE] -
    2 * p[middle, , drop = FALSE] +
    p[middle - 1L, , drop = FALSE]
  heading <- v[middle, , drop = FALSE]
  along <- rowSums(second * heading) / sqrt(rowSums(heading^2))
  along <- along[is.finite(along)]
  if (length(along) == 0L) {
    return(0)
  }
  stats::mad(along) / sqrt(6)
}

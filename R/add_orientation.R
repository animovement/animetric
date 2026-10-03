#' Declare orientation from the positions of points
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Works out which way a body faces from where its points are, and declares
#' it as the frame's orientation: `heading` in 2D, a unit quaternion
#' (`qw`, `qx`, `qy`, `qz`) in 3D. Once declared, it is a proper `where`
#' variable that later steps can use, such as egocentric alignment by
#' orientation in anispace.
#'
#' The body frame follows anicore's: an orientation of zero faces `+x`.
#' \describe{
#'   \item{Along the body (default)}{`from -> to` is the forward axis, e.g.
#'     tail to head.}
#'   \item{Across the body (`perpendicular = TRUE`)}{`from -> to` runs from
#'     the body's right to its left, e.g. right eye to left eye, and forward
#'     is perpendicular to it.}
#' }
#' In 3D, two points give a direction but not the roll about it, so a third,
#' `plane`, is needed. Any point off the line through `from` and `to` will
#' do: it fixes which way the body's `y` axis points (along the body), or its
#' forward axis (across the body). A point on the animal's left makes `y` its
#' left; a point on its back makes `y` point dorsally instead. Either way the
#' orientation is fully defined; only what its roll is called depends on the
#' choice.
#'
#' @param data A 2D or 3D anipoint with Cartesian coordinates.
#' @param from,to Members of `level` defining the axis: forward, or with
#'   `perpendicular = TRUE`, right to left.
#' @param plane In 3D, a member of `level` off the line through `from` and
#'   `to`, fixing the roll about it. Not used in 2D.
#' @param perpendicular Whether `from -> to` runs across the body, from right
#'   to left, rather than along it.
#' @param attach_to Members of `level` that get the orientation. `NULL`, the
#'   default, gives it to every member of the subject: the body's
#'   orientation. Name some to attach different orientations to different
#'   parts, e.g. a head orientation to the head's points and a body
#'   orientation to the rest.
#' @param level The identity variable `from`, `to`, `plane` and `attach_to`
#'   name members of. Defaults to the frame's only one; a frame declaring
#'   several has to be told.
#' @param name The orientation column: one name in 2D, four in 3D (`w`, `x`,
#'   `y`, `z` components). Defaults to the columns of an orientation already
#'   declared, or else `"heading"` in 2D and `c("qw", "qx", "qy", "qz")` in 3D.
#' @param overwrite Whether to replace orientation values already present in
#'   the rows being written, or an orientation declared in other columns.
#'
#' @details
#' The orientation is computed for each subject at each moment: each
#' combination of the frame's other identity variables, its temporal keys and
#' its index. Where a defining point is missing, or the points coincide, it is
#' `NA`. Rows not named by `attach_to` keep what they had, so successive calls
#' can build up orientations for different parts in the one declared column.
#'
#' In 2D, `heading` is in the frame's `unit_angle`, counting from the `x`
#' axis toward the `y` axis, in `(-pi, pi]`. Across the body, forward is the
#' `from -> to` axis turned a quarter turn, so that `to` is on the body's
#' left. In 3D the quaternion maps the body's axes into the frame's, and is
#' built with [anispace::quat_from_vectors()].
#'
#' @return The anipoint, with the orientation columns written and declared.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 5, n_individuals = 2, n_keypoints = 3)
#'
#' # The body faces from the shoulder to the head
#' add_orientation(af, from = "shoulder_right", to = "head", level = "keypoint")
#'
#' # Attached to the head only
#' add_orientation(
#'   af,
#'   from = "shoulder_right",
#'   to = "head",
#'   attach_to = "head",
#'   level = "keypoint"
#' )
#' @export
add_orientation <- function(
  data,
  from,
  to,
  plane = NULL,
  perpendicular = FALSE,
  attach_to = NULL,
  level = NULL,
  name = NULL,
  overwrite = FALSE
) {
  anicore::ensure_is_anipoint(data)
  axes <- cartesian_axes(data)
  n_axes <- length(axes)
  if (!anicore::is_cartesian(data) || !n_axes %in% c(2L, 3L)) {
    cli::cli_abort(
      "{.fn add_orientation} needs a 2D or 3D frame in Cartesian coordinates."
    )
  }
  if (!rlang::is_bool(perpendicular)) {
    cli::cli_abort(
      "{.arg perpendicular} must be {.code TRUE} or {.code FALSE}."
    )
  }
  if (!rlang::is_bool(overwrite)) {
    cli::cli_abort("{.arg overwrite} must be {.code TRUE} or {.code FALSE}.")
  }

  level <- resolve_collapsed_identity(data, level)
  if (length(level) != 1L) {
    cli::cli_abort("{.arg level} must name one identity variable.")
  }
  members <- as.character(unique(data[[level]]))
  check_members(from, members, level, "from")
  check_members(to, members, level, "to")
  if (n_axes == 2L && !is.null(plane)) {
    cli::cli_abort(c(
      "{.arg plane} is only used in 3D.",
      "i" = "In 2D, {.arg from} and {.arg to} define the orientation fully."
    ))
  }
  if (n_axes == 3L) {
    if (is.null(plane)) {
      cli::cli_abort(c(
        "In 3D, {.arg plane} is needed to fix the roll about the {.arg from}-{.arg to} axis.",
        "i" = "Any member of {.field {level}} off that line will do."
      ))
    }
    check_members(plane, members, level, "plane")
  }
  if (!is.null(attach_to)) {
    check_members(attach_to, members, level, "attach_to", single = FALSE)
  }

  declared <- anicore::get_variables(data, "where", "orientation")
  roles <- if (n_axes == 2L) "yaw" else c("qw", "qx", "qy", "qz")
  name <- resolve_orientation_name(name, declared, roles, overwrite)

  # The orientation of each subject at each moment, from its points there
  keys <- retained_grouping(data, level)
  bare <- dplyr::as_tibble(as.data.frame(data))
  point <- function(member) {
    rows <- bare[as.character(bare[[level]]) == member, c(keys, unname(axes))]
    rows[!duplicated(rows[keys]), , drop = FALSE]
  }
  ends <- dplyr::inner_join(
    point(from),
    point(to),
    by = keys,
    suffix = c(".from", ".to")
  )
  axis <- as.matrix(ends[paste0(axes, ".to")]) -
    as.matrix(ends[paste0(axes, ".from")])

  values <- if (n_axes == 2L) {
    forward <- if (perpendicular) cbind(axis[, 2], -axis[, 1]) else axis
    length_sq <- rowSums(forward^2)
    heading <- ifelse(
      !is.na(length_sq) & length_sq > 0,
      atan2(forward[, 2], forward[, 1]),
      NA_real_
    )
    data.frame(
      anicore::angle_from_rad(
        heading,
        anicore::get_metadata(data, "unit_angle")
      )
    )
  } else {
    third <- dplyr::left_join(ends[keys], point(plane), by = keys)
    towards <- as.matrix(third[unname(axes)]) -
      as.matrix(ends[paste0(axes, ".from")])
    as.data.frame(anispace::quat_from_vectors(
      axis,
      towards,
      axes = if (perpendicular) c("y", "x") else c("x", "y")
    ))
  }
  names(values) <- name
  orientation <- dplyr::bind_cols(ends[keys], values)

  # Write it into the rows it is attached to, leaving the rest as they were
  aligned <- dplyr::left_join(bare[keys], orientation, by = keys)
  target <- if (is.null(attach_to)) {
    rep(TRUE, nrow(bare))
  } else {
    as.character(bare[[level]]) %in% attach_to
  }
  existing <- intersect(name, names(bare))
  if (!overwrite && length(existing) > 0L) {
    filled <- target & rowSums(!is.na(as.matrix(bare[existing]))) > 0L
    if (any(filled)) {
      n <- sum(filled)
      cli::cli_abort(c(
        "{n} row{?s} being written already {?has/have} an orientation.",
        "i" = "Set {.code overwrite = TRUE} to replace {cli::qty(n)}{?it/them}."
      ))
    }
  }
  for (col in name) {
    current <- if (col %in% names(bare)) {
      bare[[col]]
    } else {
      rep(NA_real_, nrow(bare))
    }
    data[[col]] <- ifelse(target, aligned[[col]], current)
  }

  anicore::set_variables(
    data,
    where = list(orientation = rlang::set_names(name, roles))
  )
}

#' Check that arguments name members of a level
#'
#' @param x The argument's value.
#' @param members The level's members.
#' @param level The level's name, for the message.
#' @param arg The argument's name, for the message.
#' @param single Whether exactly one member is expected.
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_members <- function(
  x,
  members,
  level,
  arg,
  single = TRUE,
  call = rlang::caller_env()
) {
  if (single && !rlang::is_string(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single member of {.field {level}}.",
      call = call
    )
  }
  if (!is.character(x) || length(x) == 0L) {
    cli::cli_abort(
      "{.arg {arg}} must name members of {.field {level}}.",
      call = call
    )
  }
  missing <- setdiff(x, members)
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.val {missing}} {?is not a member/are not members} of {.field {level}}.",
      call = call
    )
  }
  invisible(TRUE)
}

#' The columns an orientation is written to
#'
#' @param name The `name` argument, or `NULL`.
#' @param declared The orientation already declared, role to column.
#' @param roles The roles being written: `"yaw"`, or the four quaternion
#'   roles.
#' @param overwrite Whether a declaration in other columns may be replaced.
#' @param call The calling environment, for error messages.
#' @return Character vector of column names, one per role.
#' @keywords internal
resolve_orientation_name <- function(
  name,
  declared,
  roles,
  overwrite,
  call = rlang::caller_env()
) {
  if (is.null(name)) {
    name <- if (setequal(names(declared), roles)) {
      unname(declared[roles])
    } else if (length(roles) == 1L) {
      "heading"
    } else {
      roles
    }
  }
  if (
    !is.character(name) ||
      length(name) != length(roles) ||
      anyNA(name) ||
      anyDuplicated(name)
  ) {
    cli::cli_abort(
      "{.arg name} must be {length(roles)} distinct column name{?s}.",
      call = call
    )
  }
  if (
    length(declared) > 0L &&
      !identical(unname(declared[roles]), name) &&
      !overwrite
  ) {
    cli::cli_abort(
      c(
        "The frame already declares an orientation, in {.val {unname(declared)}}.",
        "i" = "Write to those columns (the default {.arg name}), or set {.code overwrite = TRUE} to declare {.val {name}} instead."
      ),
      call = call
    )
  }
  name
}

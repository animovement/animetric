#' Add a derived point to an anipoint
#'
#' @description
#' Derives a new member of an identity level at each moment, from the members
#' it collapses, and appends it to the frame. The rest of the data is
#' returned untouched. The centroid of an animal's keypoints is the usual
#' case; `method` chooses how the point is derived.
#'
#' Which levels are collapsed is the caller's choice. On pose data for a
#' team, collapsing `"keypoint"` gives each player a point of their own;
#' `across = "individual"` gives one point per keypoint across the players;
#' and collapsing both gives the single point the whole team occupies.
#'
#' A level that did not actually vary keeps its value rather than taking the
#' new member's name — an individual's strain is still its strain, since
#' nothing was derived over it.
#'
#' The new member is an ordinary member of its level: [add_kinematics()]
#' gives it kinematics, and [summarise_aniframe()] summarises it alongside the
#' tracked points.
#'
#' @param data An anipoint with Cartesian coordinates.
#' @param across Identity variables to collapse — the dimensions the new
#'   point is derived over. Required when the frame declares more than one
#'   identity variable, since their order is not a hierarchy and there is no
#'   finest one to assume; with a single identity variable, that one is the
#'   default. Collapsing every level gives a single point per position.
#' @param method How each coordinate is derived from the members' values:
#'   \describe{
#'     \item{`"centroid"`}{The mean.}
#'     \item{`"median"`}{The median, per axis: robust to a single stray
#'       point.}
#'     \item{`"weighted"`}{The mean weighted by `confidence`, so poorly
#'       tracked points count for less. Needs a `confidence` column.}
#'     \item{a function}{Applied to each axis's values, with missing values
#'       removed, returning one number; e.g. `\(x) mean(x, trim = 0.1)`.}
#'   }
#'   Missing values are ignored; a moment where every member is missing gives
#'   `NA`.
#' @param include,exclude Values of the collapsed level to keep or leave out.
#'   Only meaningful when one level is collapsed. A midpoint is the centroid
#'   of two members: `include = c("ear_l", "ear_r")`.
#' @param name Name for the new member. Defaults to `"centroid"`, `"median"`
#'   or `"weighted_centroid"` after `method`; required when `method` is a
#'   function.
#'
#' @details
#' A declared orientation is derived too: the circular mean of the members'
#' `yaw`, in the frame's `unit_angle`, or in 3D the mean of their quaternions
#' ([anispace::quat_mean()]). It is weighted by `confidence` when `method` is
#' `"weighted"`, and unweighted otherwise. The new member has no confidence of
#' its own, so its `confidence` is `NA`.
#'
#' @return The anipoint, with the new member appended as extra rows. The
#'   collapsed identity column comes back as a factor, since it now holds a
#'   named member that an integer column could not.
#'
#' @seealso [compute_point()] for the new member's rows alone.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 3)
#'
#' # Each animal gains a centroid
#' add_point(af, across = "keypoint")
#'
#' # A median instead, robust to a stray keypoint
#' add_point(af, across = "keypoint", method = "median")
#'
#' # The midpoint of two keypoints
#' add_point(af, across = "keypoint", include = c("head", "neck"), name = "neck_head")
#'
#' # A custom rule
#' add_point(af, across = "keypoint", method = \(x) mean(x, trim = 0.1), name = "trimmed")
#'
#' # One point per keypoint, across the animals
#' add_point(af, across = "individual")
#' @export
add_point <- function(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL
) {
  append_point(
    data,
    across = across,
    method = method,
    include = include,
    exclude = exclude,
    name = name
  )
}

#' Compute a derived point of an identity level
#'
#' The point [add_point()] appends, on its own: one row per position of
#' every other identity variable, derived from the members of the collapsed
#' one.
#'
#' @inheritParams add_point
#'
#' @return An anipoint containing only the new member. Its `confidence`, if
#'   the frame has one, is `NA`.
#'
#' @examples
#' af <- anicore::example_anipoint(n_obs = 20, n_individuals = 2, n_keypoints = 3)
#'
#' # The centroid of each animal's keypoints
#' compute_point(af, across = "keypoint")
#'
#' # Their median
#' compute_point(af, across = "keypoint", method = "median")
#' @export
compute_point <- function(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL
) {
  derive_point(
    data,
    across = across,
    method = method,
    include = include,
    exclude = exclude,
    name = name
  )
}

#' Append a derived point to the frame it came from
#'
#' @inheritParams add_point
#' @param orientation Whether to derive a declared orientation for the new
#'   member, or leave it `NA` as `add_centroid()` did.
#' @param call The calling environment, for error messages.
#' @return The anipoint with the new member appended.
#' @keywords internal
append_point <- function(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL,
  orientation = TRUE,
  call = rlang::caller_env()
) {
  anicore::ensure_is_anipoint(data)
  point_method <- resolve_point_method(method, name, call = call)
  identity_cols <- resolve_collapsed_identity(data, across, call = call)

  clashes <- Filter(
    function(col) point_method$name %in% as.character(unique(data[[col]])),
    identity_cols
  )
  if (length(clashes) > 0L) {
    cli::cli_abort(
      c(
        "{.val {point_method$name}} is already a value of {.field {clashes}}.",
        "i" = "Give the new point another name with {.arg name}."
      ),
      call = call
    )
  }

  check_single_level(identity_cols, include, exclude, call = call)

  # How many points go into each new one. Collapsing several levels
  # multiplies their members together, so it is their combinations that
  # have to number at least two.
  bare <- dplyr::as_tibble(data)
  combinations <- nrow(unique(bare[identity_cols]))
  if (!is.null(include)) {
    combinations <- length(include)
  } else if (!is.null(exclude)) {
    combinations <- combinations - length(exclude)
  }

  if (combinations < 2) {
    cli::cli_abort(
      c(
        "A derived point needs at least 2 members of {.val {identity_cols}}, and this has {combinations}.",
        "i" = "Nothing would be derived."
      ),
      call = call
    )
  }

  point <- derive_point(
    data,
    across = identity_cols,
    method = method,
    include = include,
    exclude = exclude,
    name = point_method$name,
    orientation = orientation,
    call = call
  )

  # The new point is a member of the collapsed level, so that column has to
  # be able to hold a name. An identity carried as an integer -- which
  # `individual` often is -- cannot, so both sides become a factor keeping
  # the original order, with the new member last.
  as_member <- function(frame) {
    out <- dplyr::as_tibble(frame)
    for (col in identity_cols) {
      out[[col]] <- factor(
        as.character(out[[col]]),
        levels = c(as.character(unique(bare[[col]])), point_method$name)
      )
    }
    out
  }

  # Re-declared rather than re-detected: on a frame whose identity is not
  # named `keypoint`, detection injects one and strands it there (#47).
  dplyr::bind_rows(as_member(data), as_member(point)) |>
    redeclare_like(data)
}

#' Derive a point from the members of an identity level
#'
#' @inheritParams append_point
#' @return An anipoint containing only the new member.
#' @keywords internal
derive_point <- function(
  data,
  across = NULL,
  method = "centroid",
  include = NULL,
  exclude = NULL,
  name = NULL,
  orientation = TRUE,
  call = rlang::caller_env()
) {
  anicore::ensure_is_anipoint(data)

  if (!anicore::is_cartesian(data)) {
    cli::cli_abort(
      "Data must be in a Cartesian coordinate system.",
      call = call
    )
  }

  if (!is.null(include) && !is.null(exclude)) {
    cli::cli_abort(
      "Cannot specify both {.arg include} and {.arg exclude}.",
      call = call
    )
  }

  point_method <- resolve_point_method(method, name, call = call)
  weighted <- identical(point_method$kind, "weighted")
  if (weighted && !"confidence" %in% names(data)) {
    cli::cli_abort(
      c(
        "{.code method = \"weighted\"} weights by {.field confidence}, and the frame has none.",
        "i" = "Use {.code method = \"centroid\"} for an unweighted mean."
      ),
      call = call
    )
  }

  # Identity, position and the index all come from the frame's declaration.
  # A valid anipoint may carry them in columns named anything (#47).
  identity_cols <- resolve_collapsed_identity(data, across, call = call)
  space_cols <- unname(anicore::get_variables(data, "where", "position"))
  orientation_cols <- if (orientation) {
    anicore::get_variables(data, "where", "orientation")
  } else {
    character()
  }
  keep <- retained_grouping(data, identity_cols)

  check_single_level(identity_cols, include, exclude, call = call)
  if (!is.null(include)) {
    data <- dplyr::filter(data, .data[[identity_cols]] %in% include)
  } else if (!is.null(exclude)) {
    data <- dplyr::filter(data, !.data[[identity_cols]] %in% exclude)
  }

  unit <- anicore::get_metadata(data, "unit_angle")
  signed_yaw <- "yaw" %in%
    names(orientation_cols) &&
    any(data[[orientation_cols[["yaw"]]]] < 0, na.rm = TRUE)
  aggregate <- point_aggregator(point_method)

  point <- data |>
    dplyr::ungroup() |>
    dplyr::mutate(.weight = if (weighted) .data$confidence else 1) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keep))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(space_cols),
        ~ aggregate(.x, .weight)
      ),
      !!!orientation_summaries(orientation_cols, unit, signed_yaw, weighted),
      # A collapsed level takes the new member's name only where it actually
      # varied. Where every row that went into it shared one value -- the
      # strain of an individual, say -- nothing was derived over it, and
      # reporting the new name there would be a lie.
      dplyr::across(
        dplyr::all_of(identity_cols),
        \(v) {
          shared <- unique(as.character(v))
          if (length(shared) == 1L) shared else point_method$name
        }
      ),
      .groups = "drop"
    ) |>
    # Only where the source tracks it: a derived point has no confidence of
    # its own, but leaving the column out would make the frames unbindable.
    (\(d) {
      if ("confidence" %in% names(data)) {
        dplyr::mutate(d, confidence = NA_real_)
      } else {
        d
      }
    })() |>
    anicore::convert_nan_to_na() |>
    suppressMessages() |>
    suppressWarnings()

  redeclare_like(point, data)
}

#' Resolve `method` and `name` for a derived point
#'
#' @param method `"centroid"`, `"median"`, `"weighted"` or a function.
#' @param name The new member's name, or `NULL` for the method's default.
#' @param call The calling environment, for error messages.
#' @return A list with `kind` (the method's name, or `"function"`), `fun`
#'   (for a function) and `name`.
#' @keywords internal
resolve_point_method <- function(method, name, call = rlang::caller_env()) {
  defaults <- c(
    centroid = "centroid",
    median = "median",
    weighted = "weighted_centroid"
  )
  if (is.function(method)) {
    if (is.null(name)) {
      cli::cli_abort(
        "{.arg name} is required when {.arg method} is a function.",
        call = call
      )
    }
    kind <- "function"
  } else if (rlang::is_string(method) && method %in% names(defaults)) {
    kind <- method
  } else {
    cli::cli_abort(
      "{.arg method} must be {.or {.val {names(defaults)}}}, or a function.",
      call = call
    )
  }
  if (is.null(name)) {
    name <- defaults[[kind]]
  }
  if (!rlang::is_string(name) || !nzchar(name)) {
    cli::cli_abort(
      "{.arg name} must be a single, non-empty string.",
      call = call
    )
  }
  list(kind = kind, fun = if (kind == "function") method, name = name)
}

#' The function that derives one coordinate from the members' values
#'
#' @param point_method As from [resolve_point_method()].
#' @return A function of the values and their weights, returning one number.
#' @keywords internal
point_aggregator <- function(point_method) {
  switch(
    point_method$kind,
    centroid = \(v, w) mean(v, na.rm = TRUE),
    median = \(v, w) stats::median(v, na.rm = TRUE),
    weighted = weighted_mean_na,
    "function" = \(v, w) {
      value <- point_method$fun(v[!is.na(v)])
      if (!is.numeric(value) || length(value) != 1L) {
        cli::cli_abort(
          "{.arg method} must return a single number, not {.obj_type_friendly {value}}."
        )
      }
      value
    }
  )
}

#' A weighted mean that ignores missing values and weights
#'
#' @param v Numeric vector of values.
#' @param w Numeric vector of weights.
#' @return The weighted mean of the values whose value and weight are both
#'   present, or `NA` when they carry no weight.
#' @keywords internal
weighted_mean_na <- function(v, w) {
  ok <- !is.na(v) & !is.na(w)
  total <- sum(w[ok])
  if (total <= 0) {
    return(NA_real_)
  }
  sum(v[ok] * w[ok]) / total
}

#' Summaries deriving a declared orientation
#'
#' @param orientation_cols Named character vector, orientation role to
#'   column, as from `anicore::get_variables(data, "where", "orientation")`.
#' @param unit The frame's `unit_angle`.
#' @param signed_yaw Whether yaw is kept in `(-pi, pi]` rather than
#'   `[0, 2pi)`, following the input.
#' @param weighted Whether to weight by `confidence`.
#' @return A named list of quosures, for `dplyr::summarise()`.
#' @keywords internal
orientation_summaries <- function(
  orientation_cols,
  unit,
  signed_yaw,
  weighted
) {
  weights <- if (weighted) {
    rlang::quo(.data$.weight)
  } else {
    rlang::quo(NULL)
  }
  if ("yaw" %in% names(orientation_cols)) {
    col <- orientation_cols[["yaw"]]
    return(rlang::list2(
      !!col := rlang::quo(
        anicore::angle_from_rad(
          mean_direction(
            anicore::angle_to_rad(.data[[!!col]], unit),
            !!weights,
            signed = !!signed_yaw
          ),
          unit
        )
      )
    ))
  }
  if (all(c("qw", "qx", "qy", "qz") %in% names(orientation_cols))) {
    cols <- unname(orientation_cols[c("qw", "qx", "qy", "qz")])
    return(list(rlang::quo(
      mean_quaternion(dplyr::pick(dplyr::all_of(!!cols)), !!weights)
    )))
  }
  list()
}

#' The mean direction of a set of angles
#'
#' @param x Numeric vector of angles, in radians.
#' @param w Weights, or `NULL` for equal ones.
#' @param signed Whether to return the direction in `(-pi, pi]` rather than
#'   `[0, 2pi)`.
#' @return One angle, in radians; `NA` when no angle is present or they
#'   cancel out.
#' @keywords internal
mean_direction <- function(x, w = NULL, signed = TRUE) {
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  ok <- !is.na(x) & !is.na(w)
  s <- sum(w[ok] * sin(x[ok]))
  c <- sum(w[ok] * cos(x[ok]))
  if (!any(ok) || sqrt(s^2 + c^2) < 1e-12) {
    return(NA_real_)
  }
  angle <- atan2(s, c)
  if (signed || angle >= 0) angle else angle + 2 * pi
}

#' The mean of a set of unit quaternions
#'
#' @param q A data frame of quaternion components, `qw, qx, qy, qz` in that
#'   order, one row per member.
#' @param w Weights, or `NULL` for equal ones.
#' @return A one-row data frame with the same columns: the mean quaternion
#'   from [anispace::quat_mean()], or `NA` when no member has one.
#' @keywords internal
mean_quaternion <- function(q, w = NULL) {
  m <- as.matrix(q)
  ok <- stats::complete.cases(m)
  if (!is.null(w)) {
    ok <- ok & !is.na(w) & w > 0
  }
  out <- if (any(ok)) {
    anispace::quat_mean(m[ok, , drop = FALSE], weights = w[ok])
  } else {
    matrix(NA_real_, nrow = 1L, ncol = 4L)
  }
  stats::setNames(as.data.frame(matrix(out, nrow = 1L)), names(q))
}

#' Check that include and exclude name one collapsed level
#'
#' @param identity_cols The identity variables being collapsed.
#' @param include,exclude As for [add_point()].
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_single_level <- function(
  identity_cols,
  include,
  exclude,
  call = rlang::caller_env()
) {
  if ((!is.null(include) || !is.null(exclude)) && length(identity_cols) != 1L) {
    cli::cli_abort(
      c(
        "{.arg include} and {.arg exclude} name values of one level, and {length(identity_cols)} are being collapsed.",
        "i" = "Collapsing {.val {identity_cols}}; filter the frame beforehand instead."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

#' Re-declare a derived frame like the one it came from
#'
#' @param derived A data frame with the source's columns.
#' @param source The anipoint it was derived from.
#' @return An anipoint with the source's declarations and metadata.
#' @keywords internal
redeclare_like <- function(derived, source) {
  out <- anicore::as_anipoint(
    derived,
    variables_what = anicore::get_variables(source, "what"),
    variables_when = anicore::get_variables(source, "when", "keys"),
    variables_where = anicore::get_variables(source, "where", "position"),
    index = anicore::get_index(source)
  )

  # The declaration is only part of it. Sampling rate, units and the rest
  # describe the same recording and have to come across too, or a summary
  # arrives claiming to know nothing about where it came from.
  anicore::set_metadata(out, metadata = anicore::get_metadata(source))
}

#' Summarise the time series of an aniframe
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises the per-row measures of a frame over each group: a measure of
#' central tendency and of dispersion for each, one row per group. It
#' describes the measures the frame already carries — the output of
#' [calculate_kinematics()], [calculate_tortuosity()] or your own `mutate()` —
#' and computes no new ones. Any grouping is allowed, since a median of rows
#' means the same whether the rows are one keypoint's or a whole animal's.
#'
#' Properties of a trajectory as a whole, such as how far it ended up from
#' where it started, are not a statistic of any column; see
#' [summarise_path()].
#'
#' Each frame class has its own default set of measures:
#' \describe{
#'   \item{anipoint}{`speed`, `acceleration`, `turning_speed`, `turning_rate`,
#'     `turning_acceleration`, `course_elevation`, the windowed
#'     `straightness`, `sinuosity` and `emax`, and `confidence`; circular
#'     statistics for `course` and for a declared `yaw`, reported as
#'     `*_heading`.}
#'   \item{anisegment}{`length` and `confidence`.}
#'   \item{anijoint}{`angle`, with circular statistics, and `confidence`.}
#' }
#' Only the measures present are summarised. Velocity and acceleration
#' components, `course_unwrapped`, and the running totals `path_length` and
#' `cumulative_turning` are left out by default; [summarise_path()] reports
#' the totals.
#'
#' Angles are summarised with circular statistics ([anicore::circ_mean()] and
#' its siblings), so that the mean of 350 and 10 degrees is 0 rather than 180.
#' They are computed in radians and reported in the frame's `unit_angle`.
#' Only the angles animetric knows of are treated as circular; any other
#' column is summarised as a linear quantity.
#'
#' @param data An anipoint, anisegment or anijoint.
#' @param cols Character vector of columns to summarise, in place of the
#'   class's default set. A known angle among them is still summarised with
#'   circular statistics.
#' @param measures Measures of central tendency and dispersion:
#'   `"median_mad"` (default) or `"mean_sd"`.
#' @param ... Not used. Passing `type` (the `"kinematics"` and `"tortuosity"`
#'   summaries this function used to combine) is deprecated: use
#'   `summarise_aniframe()` and [summarise_path()] instead.
#'
#' @return A data frame with one row per group: the grouping columns, then
#'   `<measure>_<column>` for each measure, e.g. `median_speed` and
#'   `mad_speed`.
#'
#' @seealso [summarise_path()] for whole-trajectory measures.
#'
#' @examples
#' kin <- calculate_kinematics(
#'   anicore::example_anipoint(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' )
#' summarise_aniframe(kin)
#'
#' # Mean and standard deviation instead of median and MAD
#' summarise_aniframe(kin, measures = "mean_sd")
#'
#' # Only some measures
#' summarise_aniframe(kin, cols = c("speed", "course"))
#' @export
#' @aliases summarize_aniframe
summarise_aniframe <- function(data, ...) {
  type <- legacy_type(...)
  if (!is.null(type)) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "summarise_aniframe(type)",
      details = "Use `summarise_aniframe()` for the distribution of per-row measures and `summarise_path()` for whole-path measures."
    )
    dots <- list(...)
    measures <- if (is.null(dots$measures)) "median_mad" else dots$measures
    measures <- match.arg(measures, c("median_mad", "mean_sd"))
    return(summarise_legacy(data, type, measures))
  }
  UseMethod("summarise_aniframe")
}

#' @rdname summarise_aniframe
#' @export
summarize_aniframe <- summarise_aniframe

#' @rdname summarise_aniframe
#' @export
summarise_aniframe.anipoint <- function(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
) {
  rlang::check_dots_empty()
  yaw <- anicore::get_variables(data, "where", "orientation")["yaw"]
  summarise_measures(
    data,
    cols = cols,
    measures = match.arg(measures),
    linear = c(
      "speed",
      "acceleration",
      "turning_speed",
      "turning_rate",
      "turning_acceleration",
      "course_elevation",
      "straightness",
      "sinuosity",
      "emax",
      "confidence"
    ),
    circular = c(course = "course", heading = unname(yaw[!is.na(yaw)])),
    hint = "Add some with {.fn calculate_kinematics} or {.fn calculate_tortuosity}, or name them with {.arg cols}."
  )
}

#' @rdname summarise_aniframe
#' @export
summarise_aniframe.anisegment <- function(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
) {
  rlang::check_dots_empty()
  summarise_measures(
    data,
    cols = cols,
    measures = match.arg(measures),
    linear = c("length", "confidence")
  )
}

#' @rdname summarise_aniframe
#' @export
summarise_aniframe.anijoint <- function(
  data,
  cols = NULL,
  measures = c("median_mad", "mean_sd"),
  ...
) {
  rlang::check_dots_empty()
  angle <- anicore::get_variables(data, "where", "angle")
  summarise_measures(
    data,
    cols = cols,
    measures = match.arg(measures),
    linear = "confidence",
    circular = c(angle = unname(angle))
  )
}

#' @rdname summarise_aniframe
#' @export
summarise_aniframe.anievent <- function(data, ...) {
  cli::cli_abort(c(
    "{.fn summarise_aniframe} does not summarise anievents yet.",
    "i" = "Events are intervals rather than moments; a summary of their counts and durations is planned (animovement/animetric#91)."
  ))
}

#' @rdname summarise_aniframe
#' @export
summarise_aniframe.default <- function(data, ...) {
  cli::cli_abort(
    "{.fn summarise_aniframe} needs an anipoint, anisegment or anijoint, not {.obj_type_friendly {data}}."
  )
}

#' Summarise measures with linear or circular statistics
#'
#' @param data An aniframe.
#' @param cols Columns to summarise, or `NULL` for the defaults.
#' @param measures `"median_mad"` or `"mean_sd"`.
#' @param linear The class's default linear measures.
#' @param circular The angles the class knows of, as a named character
#'   vector: output stem to column.
#' @param hint What to suggest when there is nothing to summarise.
#' @param call The calling environment, for error messages.
#' @return A data frame with one row per group.
#' @keywords internal
summarise_measures <- function(
  data,
  cols,
  measures,
  linear,
  circular = character(),
  hint = NULL,
  call = rlang::caller_env()
) {
  if (is.null(cols)) {
    linear <- intersect(linear, names(data))
    circular <- circular[circular %in% names(data)]
  } else {
    check_summary_cols(data, cols, call = call)
    circular <- circular[circular %in% cols]
    linear <- setdiff(cols, circular)
  }

  if (length(linear) + length(circular) == 0L) {
    cli::cli_abort(
      c("There are no measures to summarise.", "i" = hint),
      call = call
    )
  }

  stats <- summary_statistics(measures)
  unit <- anicore::get_metadata(data, "unit_angle")
  circular_summaries <- list()
  for (stem in names(circular)) {
    for (stat in names(stats$circular)) {
      circular_summaries[[paste0(stat, "_", stem)]] <- rlang::quo(
        anicore::angle_from_rad(
          (!!stats$circular[[stat]])(
            anicore::angle_to_rad(.data[[!!circular[[stem]]]], unit)
          ),
          unit
        )
      )
    }
  }

  data |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(linear),
        stats$linear,
        .names = "{.fn}_{.col}"
      ),
      !!!circular_summaries,
      .groups = "drop"
    )
}

#' The statistics behind each choice of `measures`
#'
#' @param measures `"median_mad"` or `"mean_sd"`.
#' @return A list of two named lists of functions, `linear` and `circular`.
#' @keywords internal
summary_statistics <- function(measures) {
  if (measures == "median_mad") {
    list(
      linear = list(
        median = \(x) stats::median(x, na.rm = TRUE),
        mad = \(x) stats::mad(x, na.rm = TRUE)
      ),
      circular = list(median = anicore::circ_median, mad = anicore::circ_mad)
    )
  } else {
    list(
      linear = list(
        mean = \(x) mean(x, na.rm = TRUE),
        sd = \(x) stats::sd(x, na.rm = TRUE)
      ),
      circular = list(mean = anicore::circ_mean, sd = anicore::circ_sd)
    )
  }
}

#' Check the columns asked to be summarised
#'
#' @param data An aniframe.
#' @param cols Character vector of column names.
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_summary_cols <- function(data, cols, call = rlang::caller_env()) {
  if (!is.character(cols) || length(cols) == 0L) {
    cli::cli_abort(
      "{.arg cols} must be a character vector of column names.",
      call = call
    )
  }
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort("Column{?s} {.val {missing}} not found.", call = call)
  }
  grouping <- intersect(cols, dplyr::group_vars(data))
  if (length(grouping) > 0L) {
    cli::cli_abort(
      "{.val {grouping}} {?is a/are} grouping column{?s}, not {?a measure/measures}.",
      call = call
    )
  }
  numeric <- vapply(cols, \(col) is.numeric(data[[col]]), logical(1))
  if (!all(numeric)) {
    cli::cli_abort(
      "Column{?s} {.val {cols[!numeric]}} {?is/are} not numeric.",
      call = call
    )
  }
  invisible(TRUE)
}

#' The `type` argument the old `summarise_aniframe()` took
#'
#' Named, or as the first unnamed argument, as the old signature
#' `summarise_aniframe(data, type, measures)` allowed.
#'
#' @param ... The arguments after `data`.
#' @return The requested types, or `NULL` when none was given.
#' @keywords internal
legacy_type <- function(...) {
  dots <- list(...)
  type <- dots$type
  dot_names <- names(dots)
  if (is.null(dot_names)) {
    dot_names <- rep("", length(dots))
  }
  unnamed <- dots[dot_names == ""]
  if (
    is.null(type) &&
      length(unnamed) > 0L &&
      is.character(unnamed[[1]]) &&
      all(unnamed[[1]] %in% c("kinematics", "tortuosity"))
  ) {
    type <- unnamed[[1]]
  }
  if (is.null(type)) {
    return(NULL)
  }
  match.arg(type, c("kinematics", "tortuosity"), several.ok = TRUE)
}

#' What `summarise_aniframe(type = )` used to return
#'
#' @param data An anipoint.
#' @param type `"kinematics"`, `"tortuosity"` or both.
#' @param measures `"median_mad"` or `"mean_sd"`.
#' @return A data frame with one row per group.
#' @keywords internal
summarise_legacy <- function(data, type, measures) {
  summaries <- list()
  if ("kinematics" %in% type) {
    summaries$kinematics <- summarise_kinematics_legacy(data, measures)
  }
  if ("tortuosity" %in% type) {
    summaries$tortuosity <- summarise_path(data)
  }
  join_summaries(summaries, dplyr::group_vars(data))
}

#' Join multiple summary data frames
#' @keywords internal
join_summaries <- function(summaries, group_vars) {
  if (length(summaries) == 1L) {
    return(summaries[[1L]])
  }

  if (length(group_vars) == 0L) {
    return(dplyr::bind_cols(summaries))
  }

  purrr::reduce(summaries, dplyr::left_join, by = group_vars)
}

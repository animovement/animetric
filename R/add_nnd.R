#' Add the distance to the n-th nearest neighbour
#'
#' @description
#' Computes, for each point, the distance to the nearest point belonging to
#' a *different* entity — typically a different individual at the same
#' moment.
#'
#' Which columns carry time and position is read from the anipoint's
#' declared variables (see [anicore::get_variables()]). The identity columns
#' are assigned roles by you, explicitly, because "another animal" and
#' "another point on this animal" are different questions and the data
#' cannot tell which one you mean.
#'
#' @details
#' Every identity variable has one of three roles:
#'
#' * **`across`** — its value must *differ* between a point and its
#'   neighbour. This is what "another" means: `"individual"` for the
#'   nearest other animal, `"keypoint"` for the nearest other point on the
#'   same animal.
#' * **`within`** — its value must *match*. Added to the temporal context,
#'   which always applies: points are never compared across timepoints,
#'   observations, sessions or trials.
#' * unnamed — unconstrained. Any value may match any other, which is what
#'   makes the default any-keypoint-to-any-keypoint.
#'
#' `focal` and `neighbour` then restrict which points are measured *from*
#' and which are eligible to be measured *to*. Both are named lists of
#' column to permitted values, and they are independent, so asymmetric
#' questions like nose-to-tail are expressible.
#'
#' @param data An anipoint.
#' @param across Column whose value must differ between a point and its
#'   neighbour.
#' @param n Which neighbour to return (1 = nearest, 2 = second nearest).
#'   Ranked by entity, not by point: with `n = 2`, the result is the
#'   closest point on the second-nearest entity.
#' @param within Identity columns that must match, added to the temporal
#'   context.
#' @param focal Named list restricting which points are measured from, e.g.
#'   `list(keypoint = "nose")`. `NULL` measures from every point.
#' @param neighbour Named list restricting which points may be returned as
#'   a neighbour, e.g. `list(keypoint = "tail")`.
#'
#' @return The input anipoint with added columns, each named `nnd_`, then
#'   the neighbour rank `n`, then what the column holds:
#'   * `nnd_<n>_<across>` — which entity the n-th nearest neighbour belongs
#'     to (e.g. `nnd_1_individual`)
#'   * `nnd_<n>_<variable>` — the neighbour's value for each unconstrained
#'     identity variable (e.g. `nnd_1_keypoint`)
#'   * `nnd_<n>_distance` — the distance to it
#'
#'   The rank is always there, `1` included, so the nearest and
#'   second-nearest neighbours can sit side by side: call `add_nnd()` once
#'   with `n = 1` and again with `n = 2`. Because the rank sits between two
#'   underscores and is only digits, the names split unambiguously with
#'   `"^nnd_(\\d+)_(.+)$"`, even when the part after it contains underscores,
#'   as an `across` column named `track_id` gives `nnd_1_track_id`.
#'
#' @examples
#' data <- anicore::example_anipoint(
#'   n_obs = 5,
#'   n_individuals = 3,
#'   n_keypoints = 3
#' )
#'
#' # Nearest other individual, any keypoint to any keypoint
#' data |> add_nnd(across = "individual")
#'
#' # Whose neck is my head nearest to?
#' data |> add_nnd(
#'   across = "individual",
#'   focal = list(keypoint = "head"),
#'   neighbour = list(keypoint = "neck")
#' )
#'
#' # Nearest keypoint within each individual
#' data |> add_nnd(across = "keypoint", within = "individual")
#'
#' # Each keypoint to the same keypoint on the nearest other individual
#' data |> add_nnd(across = "individual", within = "keypoint")
#'
#' # Nearest and second-nearest other individual, side by side
#' data |>
#'   add_nnd(across = "individual", n = 1) |>
#'   add_nnd(across = "individual", n = 2)
#'
#' @seealso [compute_nnd()] for the vector-level function.
#' @export
add_nnd <- function(
  data,
  across,
  n = 1L,
  within = NULL,
  focal = NULL,
  neighbour = NULL
) {
  anicore::ensure_is_anipoint(data)
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 || n != round(n)) {
    cli::cli_abort(
      "{.arg n} must be a single whole number of 1 or more, not {.val {n}}."
    )
  }
  n <- as.integer(n)

  variables <- nnd_variables(data)
  ensure_nnd_column(data, across, "across")

  if (all(is.na(data[[across]]))) {
    cli::cli_abort(c(
      "Column {.val {across}} contains only {.val NA} values.",
      "i" = "At least two distinct values are needed to measure between."
    ))
  }

  context <- nnd_context(data, variables, within)
  unconstrained <- setdiff(variables$what, c(across, context))
  added <- nnd_names(across, unconstrained, n)
  clash <- intersect(added, names(data))
  if (length(clash) > 0) {
    cli::cli_abort(c(
      "{.arg data} already has {.field {clash}}.",
      "i" = "Drop {cli::qty(length(clash))}{?it/them} first, or give a different {.arg n}."
    ))
  }

  is_focal <- nnd_mask(data, focal, "focal")
  is_candidate <- nnd_mask(data, neighbour, "neighbour")

  coords <- nnd_coords(data, variables$where)
  incoming_classes <- class(data)

  # The masks are whole-column vectors, so they are attached to a bare
  # frame: on the grouped anipoint, `mutate()` would evaluate them once per
  # group and hit a length mismatch.
  prepared <- dplyr::as_tibble(data)
  prepared[[".nnd_focal"]] <- is_focal
  prepared[[".nnd_candidate"]] <- is_candidate

  result <- prepared |>
    dplyr::group_by(dplyr::across(dplyr::all_of(context))) |>
    dplyr::group_modify(
      ~ {
        nnd <- compute_nnd(
          x = .x[[coords[1]]],
          y = .x[[coords[2]]],
          z = if (length(coords) == 3) .x[[coords[3]]] else NULL,
          across = .x[[across]],
          n = n,
          is_focal = .x$.nnd_focal,
          is_candidate = .x$.nnd_candidate,
          labels = stats::setNames(
            lapply(unconstrained, function(col) .x[[col]]),
            unconstrained
          )
        )
        names(nnd) <- added
        dplyr::bind_cols(.x, nnd)
      }
    ) |>
    dplyr::ungroup() |>
    suppressWarnings() |>
    dplyr::select(-".nnd_focal", -".nnd_candidate") |>
    # Re-declare from the input rather than re-detecting, so renamed axes and
    # custom keys survive, then restore the metadata detection cannot know.
    anicore::as_anipoint(
      variables_what = anicore::get_variables(data, "what"),
      variables_when = anicore::get_variables(data, "when", "keys"),
      variables_where = anicore::get_variables(data, "where", "position"),
      index = anicore::get_index(data)
    ) |>
    anicore::set_metadata(metadata = anicore::get_metadata(data))

  outgoing_classes <- class(result)
  class(result) <- c(
    incoming_classes[!incoming_classes %in% outgoing_classes],
    outgoing_classes
  )

  result
}


#' Name the columns `add_nnd()` adds
#'
#' `nnd_`, the neighbour rank, then what the column holds, so the names
#' split with `"^nnd_(\\d+)_(.+)$"`. In the order [compute_nnd()] returns its
#' columns: which entity the neighbour belongs to, its value for each
#' unconstrained identity variable, and the distance.
#'
#' @param across,n As in [add_nnd()].
#' @param unconstrained The identity variables neither `across` nor in the
#'   context.
#' @return `nnd_<n>_<across>`, `nnd_<n>_<variable>` for each of
#'   `unconstrained`, and `nnd_<n>_distance`.
#' @keywords internal
nnd_names <- function(across, unconstrained, n) {
  paste0("nnd_", n, "_", c(across, unconstrained, "distance"))
}

#' The variable roles an anipoint declares
#'
#' @param data An anipoint.
#'
#' @return Named list of `what`, `when` and `where`, each restricted to
#'   columns actually present in the data.
#' @keywords internal
nnd_variables <- function(data) {
  present <- function(cols) intersect(unname(cols), names(data))

  # `when` includes the index: neighbours share a moment, not just a session.
  list(
    what = present(anicore::get_variables(data, "what")),
    when = present(anicore::get_variables(data, "when")),
    where = present(anicore::get_variables(data, "where", "position"))
  )
}


#' Ensure an argument names a single column present in the data
#'
#' @param data An anipoint.
#' @param value The supplied column name.
#' @param arg Argument name, for the error message.
#'
#' @return `TRUE`, invisibly.
#' @keywords internal
ensure_nnd_column <- function(data, value, arg) {
  if (!is.character(value) || length(value) != 1 || !value %in% names(data)) {
    cli::cli_abort(c(
      "{.arg {arg}} must name a single column present in the data.",
      "x" = "Got {.val {value}}.",
      "i" = "Available identity columns: {.val {names(data)}}."
    ))
  }
  invisible(TRUE)
}


#' Columns that must match between a point and its neighbours
#'
#' Every temporal variable the frame declares, plus whatever the caller
#' adds. The temporal part is not optional: comparing positions across
#' timepoints, or across observations that each start at their own time
#' origin, is never what is wanted.
#'
#' @param data An anipoint.
#' @param variables Output of [nnd_variables()].
#' @param within Extra columns supplied by the caller.
#'
#' @return Character vector of column names.
#' @keywords internal
nnd_context <- function(data, variables, within) {
  if (!is.null(within)) {
    for (col in within) {
      ensure_nnd_column(data, col, "within")
    }
  }

  context <- unique(c(variables$when, within))

  if (length(context) == 0) {
    cli::cli_abort(c(
      "No context to compare within.",
      "i" = "Neighbours are compared only where every temporal variable
             matches, and this frame declares none."
    ))
  }

  context
}


#' Turn a focal / neighbour restriction into a row mask
#'
#' @param data An anipoint.
#' @param values Named list of column to permitted values, or `NULL`.
#' @param arg Argument name, for error messages.
#'
#' @return Logical vector, one element per row.
#' @keywords internal
nnd_mask <- function(data, values, arg) {
  if (is.null(values)) {
    return(rep(TRUE, nrow(data)))
  }

  if (!is.list(values) || is.null(names(values)) || any(names(values) == "")) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a named list of column to permitted values.",
      "i" = "For example {.code {arg} = list(keypoint = \"nose\")}."
    ))
  }

  missing_cols <- setdiff(names(values), names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg {arg}} names {cli::qty(missing_cols)}column{?s} not found in the
       data: {.val {missing_cols}}."
    )
  }

  mask <- rep(TRUE, nrow(data))
  for (col in names(values)) {
    wanted <- as.character(values[[col]])
    present <- as.character(data[[col]])
    unmatched <- setdiff(wanted, unique(present))

    if (length(unmatched) == length(wanted)) {
      cli::cli_abort(c(
        "No rows match {.arg {arg}}.",
        "x" = "Column {.val {col}} has no value{?s} {.val {wanted}}.",
        "i" = "Available: {.val {unique(present)}}."
      ))
    }
    if (length(unmatched) > 0) {
      cli::cli_warn(
        "Some {.arg {arg}} values are absent from {.val {col}}:
         {.val {unmatched}}."
      )
    }

    mask <- mask & present %in% wanted
  }

  mask
}


#' Spatial columns to measure distance in
#'
#' @param data An anipoint.
#' @param where Spatial variables the frame declares.
#'
#' @return Character vector of column names.
#' @keywords internal
nnd_coords <- function(data, where) {
  system <- as.character(anicore::get_metadata(data, "coordinate_system"))

  if (!startsWith(system, "cartesian")) {
    cli::cli_abort(c(
      "Distances need Cartesian coordinates, but this data is
       {.val {system}}.",
      "i" = "Convert it first with {.fn anispace::map_to_cartesian}."
    ))
  }

  if (length(where) < 2) {
    cli::cli_abort(c(
      "At least two spatial variables are needed to measure distance.",
      "x" = "The frame declares the spatial variables {.val {where}}."
    ))
  }

  where
}

# Reading identity from the metadata rather than assuming `keypoint` (#47)
#
# `variables_what` is ordered coarse to fine -- model, individual, subject,
# track, keypoint -- so the last entry is the finest level, and the one a
# summary collapses across. A frame may declare identity any other way and
# still be a valid aniframe, so the column has to be looked up rather than
# named.

#' The finest identity variable a frame declares
#'
#' The level a keypoint-style summary collapses across. `variables_what`
#' runs coarse to fine, so it is the last entry.
#'
#' @param data An aniframe.
#'
#' @return Length-one character vector naming the column.
#' @keywords internal
finest_identity <- function(data, call = rlang::caller_env()) {
  what <- anicore::get_variables_what(data)
  if (length(what) == 0L) {
    cli::cli_abort(
      c(
        "This aniframe declares no identity variables.",
        "i" = "Summarising across identity needs at least one; see {.fn anicore::set_variables_what}."
      ),
      call = call
    )
  }
  what[[length(what)]]
}


#' The columns a summary should keep, having collapsed the finest identity
#'
#' Everything the frame groups by except the level being summarised over,
#' plus the index -- one row per remaining entity per position.
#'
#' @param data An aniframe.
#' @param collapsed The identity column being summarised across.
#'
#' @return Character vector of column names.
#' @keywords internal
retained_grouping <- function(data, collapsed) {
  unique(c(
    setdiff(anicore::get_variables_what(data), collapsed),
    anicore::get_variables_when(data),
    anicore::get_index(data)
  ))
}


#' The identity variables a summary collapses
#'
#' Which levels are summarised is the caller's to choose, and there is no
#' guessing it: `variables_what` is documented as coarse to fine, but nothing
#' enforces that and orthogonal attributes do not nest at all
#' (animovement/anicore#140, animovement/anicore#141). A frame declaring more
#' than one identity variable has to be told.
#'
#' A frame declaring exactly one has nothing to be ambiguous about.
#'
#' @param data An aniframe.
#' @param across Identity variables to collapse, or `NULL` for the finest one.
#'
#' @return Character vector naming the columns to collapse.
#' @keywords internal
resolve_collapsed_identity <- function(
  data,
  across = NULL,
  call = rlang::caller_env()
) {
  what <- anicore::get_variables_what(data)

  if (is.null(across)) {
    if (length(what) == 0L) {
      cli::cli_abort(
        c(
          "This aniframe declares no identity variables.",
          "i" = "Summarising across identity needs at least one; see {.fn anicore::set_variables_what}."
        ),
        call = call
      )
    }
    if (length(what) == 1L) {
      return(what)
    }
    cli::cli_abort(
      c(
        "This aniframe declares {length(what)} identity variables, so {.arg across} has to say which to collapse.",
        "i" = "It declares {.val {what}}.",
        "i" = "For example {.code across = {.str {what[[length(what)]]}}}."
      ),
      call = call
    )
  }

  if (!is.character(across) || length(across) == 0L) {
    cli::cli_abort("{.arg across} must name at least one column.", call = call)
  }

  unknown <- setdiff(across, what)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.val {unknown}} {?is/are} not {?an/} identity variable{?s} of this aniframe.",
        "i" = "It declares {.val {what}}.",
        "i" = "Only identity variables can be collapsed into a summary point.",
        "i" = "Collapsing a temporal variable or the index averages over time, which is what the {.fn summarise_kinematics} family does; this one adds a point at each position rather than removing them."
      ),
      call = call
    )
  }
  across
}

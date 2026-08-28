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

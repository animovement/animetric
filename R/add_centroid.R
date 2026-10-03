#' Add a centroid to an anipoint
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [add_point()], whose default `method = "centroid"`
#' does the same, and which can also derive a median, a confidence-weighted
#' centroid or a point by a rule of your own, and derives a declared
#' orientation for the new member.
#'
#' This returns exactly what it did: the centroid as a new member, with any
#' declared orientation left `NA` for it.
#'
#' @inheritParams add_point
#' @param name Name for the new member. Default is `"centroid"`.
#'
#' @return The anipoint, with the centroid appended as extra rows.
#' @keywords internal
#' @export
add_centroid <- function(
  data,
  across = NULL,
  include = NULL,
  exclude = NULL,
  name = "centroid"
) {
  lifecycle::deprecate_warn("0.6.0", "add_centroid()", "add_point()")
  append_point(
    data,
    across = across,
    method = "centroid",
    include = include,
    exclude = exclude,
    name = name,
    orientation = FALSE
  )
}

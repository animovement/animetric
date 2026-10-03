#' Compute the centroid of an identity level
#'
#' @description
#' **Deprecated.** Use [compute_point()], whose default `method = "centroid"`
#' does the same.
#'
#' This returns exactly what it did: the centroid, with any declared
#' orientation left `NA`.
#'
#' @inheritParams add_point
#' @param name Name for the summary member. Default is `"centroid"`.
#'
#' @return An anipoint containing only the centroid.
#' @keywords internal
#' @export
compute_centroid <- function(
  data,
  across = NULL,
  include = NULL,
  exclude = NULL,
  name = "centroid"
) {
  lifecycle::deprecate_warn("0.6.0", "compute_centroid()", "compute_point()")
  derive_point(
    data,
    across = across,
    method = "centroid",
    include = include,
    exclude = exclude,
    name = name,
    orientation = FALSE
  )
}

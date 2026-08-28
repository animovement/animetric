#' Compute the centroid of an identity level
#'
#' The mean position of the members of one identity variable, at each position
#' of every other. Which variable is collapsed is the caller's choice: the
#' keypoints of an animal, the animals of a team, or any other level the frame
#' declares.
#'
#' @param data An aniframe with Cartesian coordinates.
#' @param across Identity variables to collapse — the dimensions the summary
#'   ranges over. Defaults to the finest one the frame declares, which is the
#'   keypoint-style summary. Collapsing every level gives a single point per
#'   position.

#' @param include,exclude Values of the collapsed level to keep or leave out.
#'   Only meaningful when one level is collapsed.
#' @param name Name for the summary member. Default is `"centroid"`.
#'
#' @return An aniframe containing only the summary member. Coordinate values
#'   are the mean of the members included, with `NA`s removed. Confidence is
#'   `NA`. Missing coordinate dimensions return `NA`.
#'
#' @examples
#' af <- anicore::example_aniframe(n_obs = 20, n_individuals = 2, n_keypoints = 3)
#'
#' # The centroid of each animal's keypoints
#' compute_centroid(af, across = "keypoint")
#'
#' # From a subset of them
#' compute_centroid(af, across = "keypoint", include = c("head", "neck"))
#'
#' # The centroid of the animals themselves, one per keypoint
#' compute_centroid(af, across = "individual")
#'
#' # One point for the whole group, per position
#' compute_centroid(af, across = c("individual", "keypoint"))
#'
#' @seealso [add_centroid()], which appends the result to the frame.
#' @export
compute_centroid <- function(
  data,
  across = NULL,
  include = NULL,
  exclude = NULL,
  name = "centroid"
) {
  anicore::ensure_is_aniframe(data)

  if (!anicore::is_cartesian(data)) {
    cli::cli_abort("Data must be in a Cartesian coordinate system.")
  }

  if (!is.null(include) && !is.null(exclude)) {
    cli::cli_abort("Cannot specify both {.arg include} and {.arg exclude}.")
  }

  # Identity, position and the index all come from the frame's declaration.
  # A valid aniframe may carry them in columns named anything (#47).
  identity_cols <- resolve_collapsed_identity(data, across)
  space_cols <- anicore::get_variables_where(data)
  keep <- retained_grouping(data, identity_cols)

  if (!is.null(include) || !is.null(exclude)) {
    if (length(identity_cols) != 1L) {
      cli::cli_abort(c(
        "{.arg include} and {.arg exclude} name values of one level, and {length(identity_cols)} are being collapsed.",
        "i" = "Collapsing {.val {identity_cols}}; filter the frame beforehand instead."
      ))
    }
    data <- if (!is.null(include)) {
      dplyr::filter(data, .data[[identity_cols]] %in% include)
    } else {
      dplyr::filter(data, !.data[[identity_cols]] %in% exclude)
    }
  }

  centroid <- data |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keep))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(space_cols),
        \(v) mean(v, na.rm = TRUE)
      ),
      # A collapsed level takes the summary's name only where it actually
      # varied. Where every row that went into the summary shared one value
      # -- the strain of an individual, say -- nothing was averaged over it,
      # and reporting the summary name there would be a lie.
      dplyr::across(
        dplyr::all_of(identity_cols),
        \(v) {
          shared <- unique(as.character(v))
          if (length(shared) == 1L) shared else name
        }
      ),
      .groups = "drop"
    ) |>
    # Only where the source tracks it: a summary has no confidence of its
    # own, but inventing the column would make the frames unbindable.
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

  redeclare_like(centroid, data, space_cols)
}


#' Re-declare a derived frame the way its source was declared
#'
#' Detection only recognises the standard identity names, so letting
#' `as_aniframe()` re-detect gives a frame using its own names an invented
#' `keypoint` column and a replaced declaration (#47). The rest of the
#' source's metadata is carried over with it.
#'
#' @param derived A plain data frame derived from `source`.
#' @param source The aniframe it came from.
#' @param space_cols The spatial columns `derived` carries.
#'
#' @return `derived` as an aniframe, declared as `source` was.
#' @keywords internal
redeclare_like <- function(derived, source, space_cols) {
  out <- anicore::as_aniframe(
    derived,
    variables_what = anicore::get_variables_what(source),
    variables_when = anicore::get_variables_when(source),
    variables_where = space_cols,
    index = anicore::get_index(source)
  )

  # The declaration is only part of it. Sampling rate, units and the rest
  # describe the same recording and have to come across too, or a summary
  # arrives claiming to know nothing about where it came from.
  anicore::set_metadata(out, metadata = anicore::get_metadata(source))
}

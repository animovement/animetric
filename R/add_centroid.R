#' Add a centroid to an aniframe
#'
#' @description
#' Appends the centroid of one identity level to the frame, as a new member of
#' that level. The rest of the data is returned untouched.
#'
#' Which levels are collapsed is the caller's choice. On pose data for a
#' team, the default gives each player their own centre;
#' `across = "individual"` gives one centre per keypoint across the players;
#' and collapsing both gives the single point the whole team occupies.
#'
#' A level that did not actually vary keeps its value rather than taking the
#' summary's name — an individual's strain is still its strain, since nothing
#' was averaged over it.
#'
#' @param data An aniframe with Cartesian coordinates.
#' @param across Identity variables to collapse — the dimensions the summary
#'   ranges over. Defaults to the finest one the frame declares. Collapsing
#'   every level gives a single point per position.

#' @param include,exclude Values of the collapsed level to keep or leave out.
#'   Only meaningful when one level is collapsed.
#' @param name Name for the new member. Default is `"centroid"`.
#'
#' @return The aniframe, with the centroid appended as extra rows. The
#'   collapsed identity column comes back as a factor, since it now holds a
#'   named member that an integer column could not.
#'
#' @examples
#' af <- anicore::example_aniframe(n_obs = 20, n_individuals = 2, n_keypoints = 3)
#'
#' # Each animal gains a centroid keypoint
#' add_centroid(af, across = "keypoint")
#'
#' # From a subset of the keypoints
#' add_centroid(af, across = "keypoint", include = c("head", "neck"))
#'
#' # One centre per keypoint, across the animals
#' add_centroid(af, across = "individual", name = "group")
#'
#' # The single point the whole group occupies
#' add_centroid(af, across = c("individual", "keypoint"), name = "group")
#'
#' @seealso [compute_centroid()], which returns the centroid on its own.
#' @export
add_centroid <- function(
  data,
  across = NULL,
  include = NULL,
  exclude = NULL,
  name = "centroid"
) {
  anicore::ensure_is_aniframe(data)

  identity_cols <- resolve_collapsed_identity(data, across)

  clashes <- Filter(
    function(col) name %in% as.character(unique(data[[col]])),
    identity_cols
  )
  if (length(clashes) > 0L) {
    cli::cli_abort(c(
      "{.val {name}} is already a value of {.field {clashes}}.",
      "i" = "Give the summary another name with {.arg name}."
    ))
  }

  if ((!is.null(include) || !is.null(exclude)) && length(identity_cols) != 1L) {
    cli::cli_abort(c(
      "{.arg include} and {.arg exclude} name values of one level, and {length(identity_cols)} are being collapsed.",
      "i" = "Collapsing {.val {identity_cols}}; filter the frame beforehand instead."
    ))
  }

  # How many points go into each summary. Collapsing several levels
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
    cli::cli_abort(c(
      "A centroid needs at least 2 members of {.val {identity_cols}}, and this has {combinations}.",
      "i" = "Nothing would be averaged."
    ))
  }

  centroid <- compute_centroid(
    data,
    across = identity_cols,
    include = include,
    exclude = exclude,
    name = name
  )

  # The summary is a new member of the collapsed level, so that column has
  # to be able to hold a name. An identity carried as an integer -- which
  # `individual` often is -- cannot, so both sides become a factor keeping
  # the original order, with the summary last.
  as_member <- function(frame) {
    out <- dplyr::as_tibble(frame)
    for (col in identity_cols) {
      out[[col]] <- factor(
        as.character(out[[col]]),
        levels = c(as.character(unique(bare[[col]])), name)
      )
    }
    out
  }

  # Re-declared rather than re-detected: on a frame whose identity is not
  # named `keypoint`, detection injects one and strands it there (#47).
  dplyr::bind_rows(as_member(data), as_member(centroid)) |>
    redeclare_like(data, anicore::get_variables_where(data))
}

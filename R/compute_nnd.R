#' Compute nearest neighbour distances within one group
#'
#' Low-level function behind [calculate_nnd()], operating on plain vectors
#' for one group of comparable points (typically one timepoint).
#'
#' For each focal point, candidates are ranked by the value of `across`:
#' the closest candidate point is found for each distinct value, those
#' values are ranked by that distance, and the `n`-th is returned. So
#' "second nearest" means the second nearest *entity*, not the second
#' nearest point.
#'
#' @param x,y Numeric vectors of coordinates.
#' @param z Numeric vector of coordinates, or `NULL` for 2D data.
#' @param across Vector whose value must differ between a focal point and
#'   its neighbour — the thing being ranked, e.g. individual identity.
#' @param n Which neighbour to return (1 = nearest, 2 = second nearest).
#' @param is_focal Logical vector marking which points to measure from, or
#'   `NULL` for all of them. Non-focal points get `NA` results.
#' @param is_candidate Logical vector marking which points may be returned
#'   as a neighbour, or `NULL` for all of them.
#' @param labels Named list of vectors describing each point. The matched
#'   neighbour's value is reported for each, as `nnd_<name>`.
#'
#' @return A tibble with `nnd_across` (the value of `across` for the
#'   matched neighbour), one `nnd_<name>` column per entry of `labels`,
#'   and `nnd_distance`.
#'
#' @seealso [calculate_nnd()] for the aniframe-level function.
#'
#' @examples
#' # Nearest point belonging to a different individual
#' compute_nnd(
#'   x = c(0, 1, 2),
#'   y = c(0, 1, 0),
#'   across = c("a", "b", "c")
#' )
#'
#' # The second nearest, with the neighbour's label reported back
#' compute_nnd(
#'   x = c(0, 1, 2),
#'   y = c(0, 1, 0),
#'   across = c("a", "b", "c"),
#'   n = 2L,
#'   labels = list(individual = c("a", "b", "c"))
#' )
#' @export
compute_nnd <- function(
  x,
  y,
  z = NULL,
  across,
  n = 1L,
  is_focal = NULL,
  is_candidate = NULL,
  labels = NULL
) {
  n_points <- length(x)
  across_chr <- as.character(across)

  if (is.null(is_focal)) {
    is_focal <- rep(TRUE, n_points)
  }
  if (is.null(is_candidate)) {
    is_candidate <- rep(TRUE, n_points)
  }

  coords <- if (is.null(z)) cbind(x, y) else cbind(x, y, z)
  dist_mat <- as.matrix(stats::dist(coords))

  # `stats::dist()` does not return NA for a partly-missing coordinate: it
  # drops the missing dimension and scales the remaining ones up, so a
  # point with `x = NA` would report a plausible distance computed from
  # `y` alone. A point without a full set of coordinates has no position,
  # so it is neither measured from nor offered as a neighbour.
  positioned <- stats::complete.cases(coords)
  is_focal <- is_focal & positioned
  is_candidate <- is_candidate & positioned

  result_distance <- rep(NA_real_, n_points)
  result_across <- across[rep(NA_integer_, n_points)]
  result_labels <- lapply(labels, function(v) v[rep(NA_integer_, n_points)])

  candidate_values <- unique(across_chr[is_candidate & !is.na(across_chr)])

  for (i in seq_len(n_points)) {
    if (!is_focal[i] || is.na(across_chr[i])) {
      next
    }

    dists <- dist_mat[i, ]
    other_values <- setdiff(candidate_values, across_chr[i])
    if (length(other_values) < n) {
      next
    }

    # Closest candidate point for each distinct value of `across`. Every
    # value in `candidate_values` has at least one positioned candidate
    # point by construction, so each lookup yields a finite distance.
    closest <- lapply(other_values, function(value) {
      points <- which(across_chr == value & is_candidate)
      point_dists <- dists[points]
      nearest <- which.min(point_dists)
      list(dist = point_dists[nearest], idx = points[nearest])
    })

    min_dists <- vapply(closest, function(hit) hit$dist, numeric(1))
    min_idxs <- vapply(closest, function(hit) hit$idx, integer(1))

    nth <- order(min_dists)[n]
    matched <- min_idxs[nth]

    result_distance[i] <- min_dists[nth]
    result_across[i] <- across[matched]
    for (nm in names(result_labels)) {
      result_labels[[nm]][i] <- labels[[nm]][matched]
    }
  }

  out <- dplyr::tibble(nnd_across = result_across)
  for (nm in names(result_labels)) {
    out[[paste0("nnd_", nm)]] <- result_labels[[nm]]
  }
  out$nnd_distance <- result_distance
  out
}

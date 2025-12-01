#' Compute nearest neighbour distances for a single time point
#'
#' Low-level function that computes distances to the nth nearest individual.
#' For each focal point, finds the closest point belonging to the nth nearest
#' individual (ranked by minimum distance). Called by [calculate_nnd()] for
#' each time point.
#'
#' @param x Numeric vector of x coordinates.
#' @param y Numeric vector of y coordinates.
#' @param z Numeric vector of z coordinates, or NULL for 2D data.
#' @param individual Factor or vector identifying which individual each point
#'   belongs to.
#' @param keypoint Factor or vector identifying keypoint labels, or NULL if no
#'   keypoints.
#' @param n Which individual to find (1 = nearest, 2 = second nearest, etc.).
#' @param keypoint_neighbour Character vector of keypoint(s) to consider as
#'   valid neighbours, or NULL to consider all.
#'
#' @return A tibble with columns:
#'   - `nnd_individual`: individual ID of the n-th nearest individual
#'   - `nnd_keypoint`: keypoint of the closest point on that individual (only
#'     if `keypoint` is not NULL)
#'   - `nnd_distance`: distance to the closest point on the n-th nearest
#'     individual
#'
#' @seealso [calculate_nnd()] for the user-facing aniframe function
#'
#' @export
compute_nnd <- function(
    x,
    y,
    z = NULL,
    individual,
    keypoint = NULL,
    n = 1L,
    keypoint_neighbour = NULL
) {
  n_points <- length(x)

  # Convert to character for consistent comparison
  individual_chr <- as.character(individual)

  if (is.null(keypoint) || is.null(keypoint_neighbour)) {
    is_candidate <- rep(TRUE, n_points)
  } else {
    is_candidate <- keypoint %in% keypoint_neighbour
  }

  if (is.null(z)) {
    coords <- cbind(x, y)
  } else {
    coords <- cbind(x, y, z)
  }

  dist_mat <- as.matrix(stats::dist(coords))

  # Initialise result vectors
  result_distance <- rep(NA_real_, n_points)
  result_neighbour <- individual[rep(NA_integer_, n_points)]
  result_neighbour_keypoint <- if (!is.null(keypoint)) {
    keypoint[rep(NA_integer_, n_points)]
  } else {
    NULL
  }

  # Get unique individuals (excluding NA)
  unique_inds <- unique(individual_chr[!is.na(individual_chr)])

  for (i in seq_len(n_points)) {
    focal_ind <- individual_chr[i]

    # Skip if focal individual is NA
    if (is.na(focal_ind)) next

    dists <- dist_mat[i, ]

    # Other individuals (not the focal)
    other_inds <- setdiff(unique_inds, focal_ind)

    if (length(other_inds) < n) next

    # For each other individual, find the closest candidate point
    ind_info <- lapply(other_inds, function(other_ind) {
      other_points <- which(individual_chr == other_ind & is_candidate)
      if (length(other_points) == 0) {
        return(list(dist = Inf, idx = NA_integer_))
      }
      other_dists <- dists[other_points]
      min_pos <- which.min(other_dists)
      list(dist = other_dists[min_pos], idx = other_points[min_pos])
    })

    min_dists <- vapply(ind_info, function(x) x$dist, numeric(1))
    min_idxs <- vapply(ind_info, function(x) x$idx, integer(1))

    # Find nth nearest individual
    valid_pos <- which(is.finite(min_dists))
    if (length(valid_pos) < n) next

    ranked <- valid_pos[order(min_dists[valid_pos])]
    nth_pos <- ranked[n]
    nth_point_idx <- min_idxs[nth_pos]

    result_distance[i] <- min_dists[nth_pos]
    result_neighbour[i] <- individual[nth_point_idx]
    if (!is.null(keypoint)) {
      result_neighbour_keypoint[i] <- keypoint[nth_point_idx]
    }
  }

  result <- dplyr::tibble(
    nnd_individual = result_neighbour,
    nnd_distance = result_distance
  )

  if (!is.null(keypoint)) {
    result <- dplyr::tibble(
      nnd_individual = result_neighbour,
      nnd_keypoint = result_neighbour_keypoint,
      nnd_distance = result_distance
    )
  }

  result
}

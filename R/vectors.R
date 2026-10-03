#' The Cartesian axes of a frame, and the columns carrying them
#'
#' @param data An anipoint.
#' @return Named character vector, axis role to column, for the roles `x`, `y`
#'   and `z` that the frame declares, in that order.
#' @keywords internal
cartesian_axes <- function(data) {
  axes <- anicore::get_axes(data)
  axes[intersect(c("x", "y", "z"), names(axes))]
}

#' Euclidean norm of vectors given by their components
#'
#' @param components A list or data frame of numeric vectors, one per axis.
#' @return Numeric vector, the length of each row's vector.
#' @keywords internal
vector_norm <- function(components) {
  sqrt(Reduce(`+`, lapply(components, \(x) x^2)))
}

#' Distance moved since the previous row
#'
#' @param position A list or data frame of coordinate vectors, one per axis.
#' @return Numeric vector, `NA` in the first row.
#' @keywords internal
step_length <- function(position) {
  vector_norm(lapply(position, \(x) x - dplyr::lag(x)))
}

#' Cosine of the turning angle between successive velocity vectors
#'
#' The angle between each row's velocity and the previous row's, from
#' [anicore::angle_between()]. It is `NA` in the first row and wherever either
#' velocity is zero or missing. A 1D velocity is treated as lying in a plane,
#' so a reversal is a turn of `pi`.
#'
#' @param velocity A list or data frame of velocity components, one per axis.
#' @return Numeric vector.
#' @keywords internal
cos_turning <- function(velocity) {
  v <- as.matrix(as.data.frame(velocity))
  if (nrow(v) < 2L) {
    return(rep(NA_real_, nrow(v)))
  }
  if (ncol(v) == 1L) {
    v <- cbind(v, 0)
  }
  previous <- rbind(NA_real_, v[-nrow(v), , drop = FALSE])
  cos(anicore::angle_between(previous, v))
}

#' Pad vectors given by their components to three dimensions
#'
#' @param components A list or data frame of numeric vectors, one per axis,
#'   two or three of them.
#' @return A numeric matrix with three columns, missing axes filled with 0.
#' @keywords internal
as_3d <- function(components) {
  m <- as.matrix(as.data.frame(components))
  cbind(m, matrix(0, nrow(m), 3L - ncol(m)))
}

#' Row-wise cross product of two three-column matrices
#'
#' @param u,v Numeric matrices with three columns.
#' @return A numeric matrix with three columns.
#' @keywords internal
cross_rows <- function(u, v) {
  cbind(
    u[, 2] * v[, 3] - u[, 3] * v[, 2],
    u[, 3] * v[, 1] - u[, 1] * v[, 3],
    u[, 1] * v[, 2] - u[, 2] * v[, 1]
  )
}

#' Cumulative turning of a sequence of velocity vectors
#'
#' Starts at 0 and accumulates the angle between consecutive velocities where
#' the animal is moving, so turning across a pause is counted once, when it
#' moves off again. Stationary or missing rows carry the running total.
#'
#' @param v Numeric matrix of velocities, one row per observation.
#' @param moving Logical vector, whether each row's velocity is defined and
#'   non-zero.
#' @return Numeric vector, in radians.
#' @keywords internal
cumulative_turning <- function(v, moving) {
  steps <- numeric(nrow(v))
  idx <- which(moving)
  if (length(idx) > 1L) {
    steps[idx[-1]] <- anicore::angle_between(
      v[idx[-length(idx)], , drop = FALSE],
      v[idx[-1], , drop = FALSE]
    )
  }
  cumsum(steps)
}

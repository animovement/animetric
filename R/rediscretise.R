# Rediscretising a path to a constant step length (#104)
#
# Sinuosity is defined for a path of constant step length (Benhamou 2004).
# Walking along the recorded path and placing a point wherever it first
# leaves a circle of the step length around the last point gives such a
# path, and jitter that stays within that circle gives no steps at all.

#' Rediscretise a path to a constant step length
#'
#' Walks along the path, as straight lines between the recorded positions,
#' and places a point where it first leaves a circle of radius `step_length`
#' around the last point placed (Bovet & Benhamou 1988). Missing positions
#' break the path: each unbroken stretch is rediscretised on its own.
#'
#' @param position A data frame of positions, one column per axis.
#' @param time The index, for the time at which the path reaches each new
#'   point.
#' @param step_length The step length, in the unit of the positions.
#' @return A list of `position` (a matrix, one row per new point), `time`,
#'   and `run` (which unbroken stretch each point belongs to).
#' @keywords internal
rediscretise_path <- function(position, time, step_length) {
  p <- as.matrix(as.data.frame(position))
  empty <- list(
    position = p[0, , drop = FALSE],
    time = numeric(0),
    run = integer(0)
  )
  if (!is.finite(step_length) || step_length <= 0) {
    return(empty)
  }

  complete <- stats::complete.cases(p)
  runs <- rle(complete)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L
  pieces <- list()
  for (r in which(runs$values & runs$lengths >= 2L)) {
    rows <- starts[r]:ends[r]
    piece <- rediscretise_run(p[rows, , drop = FALSE], time[rows], step_length)
    piece$run <- rep(r, length(piece$time))
    pieces[[length(pieces) + 1L]] <- piece
  }
  if (length(pieces) == 0L) {
    return(empty)
  }
  list(
    position = do.call(rbind, lapply(pieces, `[[`, "position")),
    time = unlist(lapply(pieces, `[[`, "time")),
    run = unlist(lapply(pieces, `[[`, "run"))
  )
}

#' Rediscretise one unbroken stretch of path
#'
#' @param p A numeric matrix of positions, no missing values, at least two
#'   rows.
#' @param time The index.
#' @param step_length The step length, positive.
#' @return A list of `position` and `time` of the new points, starting with
#'   the first recorded one.
#' @keywords internal
rediscretise_run <- function(p, time, step_length) {
  n <- nrow(p)
  # Each step covers at least step_length of path, which bounds the count;
  # one more allows for rounding when the path is a whole number of steps
  path_length <- sum(sqrt(rowSums(diff(p)^2)))
  size <- floor(path_length / step_length) + 2L
  out <- matrix(NA_real_, size, ncol(p))
  out_time <- numeric(size)
  current <- p[1, ]
  out[1, ] <- current
  out_time[1] <- time[1]
  k <- 1L
  j <- 2L
  r2 <- step_length^2
  while (j <= n) {
    if (sum((p[j, ] - current)^2) < r2) {
      j <- j + 1L
      next
    }
    # The path leaves the circle on the segment from p[j - 1] to p[j]: the
    # larger root is the crossing ahead of the current point
    start <- p[j - 1L, ]
    segment <- p[j, ] - start
    offset <- start - current
    a <- sum(segment^2)
    b <- 2 * sum(offset * segment)
    c <- sum(offset^2) - r2
    t <- (-b + sqrt(max(b^2 - 4 * a * c, 0))) / (2 * a)
    current <- start + t * segment
    k <- k + 1L
    out[k, ] <- current
    out_time[k] <- time[j - 1L] + t * (time[j] - time[j - 1L])
  }
  list(
    position = out[seq_len(k), , drop = FALSE],
    time = out_time[seq_len(k)]
  )
}

#' Turning along a rediscretised path
#'
#' @param path A rediscretised path, from [rediscretise_path()].
#' @return A data frame with one row per point between two steps of the same
#'   stretch: `time`, when the path reaches the point, and `cos_turning`, the
#'   cosine of the angle between the steps either side of it.
#' @keywords internal
rediscretised_turning <- function(path) {
  m <- nrow(path$position)
  if (m < 3L) {
    return(data.frame(time = numeric(0), cos_turning = numeric(0)))
  }
  steps <- diff(path$position)
  before <- steps[-nrow(steps), , drop = FALSE]
  after <- steps[-1L, , drop = FALSE]
  cos_turning <- rowSums(before * after) /
    sqrt(rowSums(before^2) * rowSums(after^2))
  middle <- 2:(m - 1L)
  same_run <- path$run[middle - 1L] == path$run[middle + 1L]
  data.frame(
    time = path$time[middle][same_run],
    cos_turning = pmin(pmax(cos_turning[same_run], -1), 1)
  )
}

#' The step length to rediscretise a trajectory at
#'
#' The mean step between rows, weighted by its length: the average step
#' over the distance travelled rather than over time. Time spent still adds
#' many short steps, which would shorten a plain mean however long the
#' animal paused, but adds almost nothing to this one.
#'
#' @param position A data frame of positions, one column per axis.
#' @return A number, ignoring steps to or from a missing position. `NaN`
#'   when the trajectory never moves.
#' @keywords internal
mean_step_length <- function(position) {
  step <- step_length(position)
  sum(step^2, na.rm = TRUE) / sum(step, na.rm = TRUE)
}

#' The step length to rediscretise a trajectory at, as asked for
#'
#' @param step_length See [summarise_path()], already checked.
#' @param position A data frame of positions, one column per axis.
#' @return A number: `step_length` itself, or for `"auto"` the trajectory's
#'   [mean_step_length()].
#' @keywords internal
resolve_step_length <- function(step_length, position) {
  if (identical(step_length, "auto")) {
    return(mean_step_length(position))
  }
  step_length
}

#' Check a `step_length` argument
#'
#' @param step_length See [summarise_path()].
#' @param call The calling environment, for error messages.
#' @return `TRUE`, invisibly.
#' @keywords internal
check_step_length <- function(step_length, call = rlang::caller_env()) {
  valid <- identical(step_length, "auto") ||
    (is.numeric(step_length) &&
      length(step_length) == 1L &&
      is.finite(step_length) &&
      step_length > 0)
  if (!valid) {
    cli::cli_abort(
      "{.arg step_length} must be {.val auto} or a single positive number.",
      call = call
    )
  }
  invisible(TRUE)
}

#' Sinuosity and E_max of a rediscretised path
#'
#' @param position A data frame of positions, one column per axis.
#' @param time The index.
#' @param step_length The step to rediscretise at, as for
#'   [resolve_step_length()].
#' @return A list of `sinuosity` and `e_max`, each a number.
#' @keywords internal
path_sinuosity <- function(position, time, step_length = "auto") {
  step <- resolve_step_length(step_length, position)
  turning <- rediscretised_turning(rediscretise_path(position, time, step))
  mean_cos <- mean(turning$cos_turning)
  list(
    sinuosity = compute_sinuosity(step, mean_cos, method = "corrected"),
    e_max = compute_emax(mean_cos)
  )
}

#' Sinuosity and E_max of a rediscretised path, over sliding windows
#'
#' The path is rediscretised once, at the step from
#' [resolve_step_length()]: by default the trajectory's step length from
#' [mean_step_length()].
#' Each row's window spans the same rows as its straightness, and takes the
#' turning at the rediscretised points the path reaches within it.
#'
#' @param position A data frame of positions, one column per axis.
#' @param time The index.
#' @param window_width The window width, in rows.
#' @param step_length The step to rediscretise at, as for
#'   [resolve_step_length()].
#' @return A list of two numeric vectors, `sinuosity` and `e_max`. `NA`
#'   where the window runs past either end, or holds no turning.
#' @keywords internal
window_sinuosity <- function(
  position,
  time,
  window_width,
  step_length = "auto"
) {
  n <- length(time)
  half_w <- window_width %/% 2L
  other_half <- window_width - half_w - 1L
  step <- resolve_step_length(step_length, position)
  turning <- rediscretised_turning(rediscretise_path(position, time, step))

  from <- dplyr::lag(time, n = half_w)
  to <- dplyr::lead(time, n = other_half)
  # Points reached in [from, to]: those after the first index below `from`,
  # up to the last at or before `to`
  first <- findInterval(from, turning$time, left.open = TRUE) + 1L
  last <- findInterval(to, turning$time)
  count <- last - first + 1L
  cumulative <- c(0, cumsum(turning$cos_turning))
  mean_cos <- (cumulative[last + 1L] - cumulative[first]) / count
  mean_cos[is.na(from) | is.na(to) | count < 1L] <- NA_real_
  # Summing in a different order can carry a mean of ones past 1
  mean_cos <- pmin(pmax(mean_cos, -1), 1)

  list(
    sinuosity = compute_sinuosity(rep(step, n), mean_cos, method = "corrected"),
    e_max = compute_emax(mean_cos)
  )
}

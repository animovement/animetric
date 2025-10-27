#' Calculate straightness measures
#'
#' @param data Data frame
#' @param straightness Which method to calculate path straightness. Choose between "A" (default), "B", "C"... or a combination (e.g. "c("A","B")"). See description for details about the different calculations.
#'
#' @keywords internal
#' @export
calculate_straightness <- function(data, straightness = c("A", "B", "C", "D")) {
  data <- data |>
    dplyr::mutate(
      straightness_A = calculate_straightness_A(
        .data$last_x,
        .data$last_y,
        .data$total_d_translation
      ),
      straightness_B = calculate_straightness_B(
        .data$last_x,
        .data$last_y,
        .data$total_d_translation
      ),
      straightness_C = calculate_straightness_C(
        .data$total_d_translation,
        .data$total_d_rotation
      ),
      straightness_D = calculate_straightness_D(
        .data$total_d_translation,
        .data$total_d_rotation
      )
    )

  # Only select the methods chosen by the user
  possible_straightness <- c("A", "B", "C", "D")
  possible_straightness_columns <- paste(
    "straightness",
    possible_straightness,
    sep = "_"
  )
  straightness_columns <- paste("straightness", straightness, sep = "_")
  columns_not_selected <- possible_straightness_columns[which(
    !possible_straightness_columns %in% straightness_columns
  )]
  data <- data |>
    dplyr::select(!dplyr::all_of(columns_not_selected))

  return(data)
}

#' @keywords internal
calculate_straightness_A <- function(x, y, distance) {
  calculate_distance(x, y) / distance
}

#' @keywords internal
calculate_straightness_B <- function(x, y, distance) {
  distance / calculate_distance(x, y)
}

#' @keywords internal
calculate_straightness_C <- function(distance, rotation) {
  distance / as.numeric(rotation)
}

#' @keywords internal
calculate_straightness_D <- function(distance, rotation) {
  as.numeric(rotation) / distance
}

#' Fractal dimension (box‑counting) -------------------------------------------------
#' @description Approximate the box‑counting fractal dimension of a 2‑D trajectory.
#' @param x numeric vector of x‑coordinates (ordered along the path)
#' @param y numeric vector of y‑coordinates (ordered along the path)
#' @param eps numeric vector of box sizes to evaluate (default: powers of 2)
#' @return numeric scalar – estimated fractal dimension.
#' @keywords internal
#' calculate_straightness_E <- function(x, y,
#'                                      eps = 2 ^ seq(-5, 5, by = 1)) {
#'   stopifnot(length(x) == length(y))
#'   # Shift to positive quadrant to avoid negative indices
#'   xs <- x - min(x, na.rm = TRUE) + 1
#'   ys <- y - min(y, na.rm = TRUE) + 1
#'
#'   n_boxes <- sapply(eps, function(e) {
#'     # Discretise coordinates to grid of size e
#'     gx <- floor(xs / e)
#'     gy <- floor(ys / e)
#'     # Count unique occupied cells
#'     length(unique(paste(gx, gy, sep = "_")))
#'   })
#'
#'   # Linear regression on log‑log plot
#'   fit <- lm(log(n_boxes) ~ log(1 / eps))
#'   as.numeric(coef(fit)[2])   # slope = fractal dimension
#' }
#'
#' #' Mean‑step‑ratio (MSR) -----------------------------------------------------------
#' #' @description Ratio of mean step length to mean absolute turning angle.
#' #' @param x numeric vector of x‑coordinates (ordered)
#' #' @param y numeric vector of y‑coordinates (ordered)
#' #' @return numeric scalar – MSR.
#' #' @keywords internal
#' calculate_straightness_F <- function(x, y) {
#'   stopifnot(length(x) == length(y))
#'   # Step vectors
#'   dx <- diff(x)
#'   dy <- diff(y)
#'   step_len <- sqrt(dx^2 + dy^2)
#'
#'   # Headings and turning angles
#'   headings <- atan2(dy, dx)
#'   turn_angles <- diff(headings)
#'   # Unwrap to get absolute change
#'   turn_abs <- abs(turn_angles)
#'
#'   mean(step_len, na.rm = TRUE) / mean(turn_abs, na.rm = TRUE)
#' }
#'
#' #' Path curvature (κ) -------------------------------------------------------------
#' #' @description Average curvature per unit distance (sum |Δθ| / total length).
#' #' @param total_translation numeric – total path length (sum of step lengths)
#' #' @param total_rotation    numeric – total absolute turning angle (radians)
#' #' @return numeric scalar – curvature κ.
#' #' @keywords internal
#' calculate_straightness_G <- function(total_translation, total_rotation) {
#'   total_rotation / total_translation
#' }

#' Descriptive statistics for circular (angular) data
#'
#' This script implements a small toolbox for computing common
#' descriptive measures on angles expressed in radians.  All functions
#' assume inputs are numeric vectors and automatically wrap values to
#' the interval [0, 2*pi).  The quantile routine now follows the
#' Fisher‑&‑Lee (1995) algorithm, which yields true circular
#' quantiles (the 0.5‑quantile coincides with the circular median).
#'
#' @examples
#' angles <- runif(200, 0, 2*pi)
#' circ_mean(angles)
#' circ_sd(angles)
#' circ_median(angles)
#' circ_mad(angles)
#' circ_quantile(angles, probs = c(0.25, 0.5, 0.75))
NULL

circ_check <- function(){
  # Check that circular is installed
  rlang::check_installed(
    "circular",
    reason = "for calculating circular descriptive statistics,",
    action = function(...) {
      utils::install.packages(
        'circular',
        repos = c(
          'https://animovement.r-universe.dev',
          'https://cloud.r-project.org'
        )
      )
    }
  )
}

# -------------------------------------------------------------------------
# Central tendency ---------------------------------------------------------

#' Circular mean
#' @param x Numeric vector of angles in radians
#' @param na_rm Logical; if TRUE, remove NA values before computation
#' @return Mean direction (radians) wrapped to [0, 2*pi)
circ_mean <- function(x, na_rm = TRUE) {
  circ_check()

  # Handle missing values
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_warn("No observations (at least after removing missing values)")
    return(NA_real_)
  }

  # Use the circular implementation
  x |>
    circular::circular() |>
    circular::mean.circular() |>
    as.numeric()
}

#' Calculate circular median
#'
#' Computes the median of circular data using Fisher's definition.
#'
#' @param x Numeric vector of angles in radians
#' @param na_rm Logical; if TRUE, remove NA values before computation
#'
#' @return The circular median as a single numeric value in radians
#'
#' @examples
#' # Angles in radians
#' angles <- c(0.1, 0.2, 6.2, 6.3)
#' circ_median(angles)
circ_median <- function(x, na_rm = TRUE) {
  circ_check()

  # Handle missing values
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_warn("No observations (at least after removing missing values)")
    return(NA_real_)
  }

  # Use the circular implementation
  # Use the circular implementation
  y <- x |>
    constrain_angles_radians() |>
    circular::circular() |>
    circular::median.circular()

  attributes(y)$medians |>
    as.numeric() |>
    mean()
}

# -------------------------------------------------------------------------
# Dispersion --------------------------------------------------------------

#' Circular standard deviation
#' @param x Numeric vector of angles (radians)
#' @param na_rm Logical; if TRUE, remove NA values before computation
#' @return Circular SD (radians)
circ_sd <- function(x, na_rm = TRUE) {
  circ_check()

  # Handle missing values
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_warn("No observations (at least after removing missing values)")
    return(NA_real_)
  }

  # Use the circular implementation
  x |>
    circular::circular() |>
    circular::sd.circular() |>
    as.numeric()
}

#' Circular median absolute deviation
#' @param x Numeric vector of angles (radians)
#' @param na_rm Logical; if TRUE, remove NA values before computation
#' @return MAD (radians)
circ_mad <- function(x, na_rm = TRUE) {
  circ_check()

  # Handle missing values
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_warn("No observations (at least after removing missing values)")
    return(NA_real_)
  }

  # Compute circular median if not provided
  center <- circ_median(x)

  # Calculate absolute angular deviations from the median
  deviations <- abs(calculate_angular_difference(center, x))

  # Return the median of these deviations
  stats::median(deviations)
}

#' Circular quantiles (Fisher & Lee 1995)
#'
#' Returns the angle θ such that a proportion *p* of the data lie
#' clockwise from θ.  The algorithm works directly on the circle, so it
#' is immune to the artificial break at 0/2π that affects simple
#' rank‑based methods.  Consequently the 0.5‑quantile coincides with the
#' circular median.
#'
#' @param x    Numeric vector of angles (radians)
#' @param probs Numeric vector of probabilities in \[0,1\].
#' @param na_rm Logical; if TRUE, remove NA values before computation
#' @return Vector of quantile angles (radians) wrapped to \[0, 2π).
#' @references Fisher, N. I. & Lee, A. J. (1995). *Statistical Analysis
#'   of Circular Data*. Cambridge University Press. Chapter 3.
circ_quantile <- function(x, probs = c(0.25, 0.5, 0.75), na_rm = TRUE) {
  circ_check()

  # Handle missing values
  if (na_rm) {
    x <- x[!is.na(x)]
  }

  # Check for empty input
  if (length(x) == 0) {
    cli::cli_warn("No observations (at least after removing missing values)")
    return(NA_real_)
  }

  # Use the circular implementation
  x |>
    circular::circular() |>
    circular::quantile.circular(probs = probs) |>
    as.numeric()
}

# ============================================================================
# COMPUTE FUNCTIONS (vector in, vector out)
# ============================================================================

#' Compute straightness index from precomputed vectors
#'
#' @param displacement Numeric vector of net displacements (D)
#' @param path_length Numeric vector of path lengths (L)
#'
#' @return Numeric vector of straightness values (D/L)
#' @export
compute_straightness <- function(displacement, path_length) {
  straightness <- displacement / path_length
  straightness[path_length == 0 | is.na(path_length)] <- NA_real_
  straightness
}

#' Compute sinuosity index from precomputed vectors
#'
#' @param mean_step_length Numeric vector of mean step lengths within window
#' @param mean_cos_turning Numeric vector of mean cosine of turning angles
#' @param method Either "corrected" (Benhamou 2004) or "original" (Bovet & Benhamou 1988)
#'
#' @return Numeric vector of sinuosity values
#' @export
compute_sinuosity <- function(
  mean_step_length,
  mean_cos_turning,
  method = c("corrected", "original")
) {
  method <- match.arg(method)

  # Identify invalid values before computation
  invalid <- mean_cos_turning > 1 |
    is.na(mean_cos_turning) |
    mean_step_length <= 0 |
    is.na(mean_step_length)

  # Replace invalid with safe values for computation (will be NA'd later
  safe_cos <- dplyr::if_else(invalid, 0, mean_cos_turning)
  safe_step <- dplyr::if_else(invalid, 1, mean_step_length)

  if (method == "corrected") {
    # Benhamou 2004, Equation 8
    sinuosity <- 2 / sqrt(safe_step * (1 + safe_cos) / (1 - safe_cos))
  } else {
    # Original Bovet & Benhamou 1988
    sinuosity <- 2 / sqrt(safe_step * (1 - safe_cos))
  }

  sinuosity[invalid] <- NA_real_

  sinuosity
}

#' Compute E_max (maximum expected displacement) from pre‑computed vectors
#'
#' @param mean_cos_turning   Numeric vector of mean cosine of turning angles.
#' @param mean_step_length   Numeric vector of mean step lengths (required if \code{dimensional = TRUE}).
#' @param dimensional        Logical. If \code{TRUE}, returns E_max in spatial units;
#'                           otherwise returns the dimensionless ratio.
#' @return Numeric vector of E_max values (same length as \code{mean_cos_turning}),
#'         with \code{NA} for invalid inputs and \code{Inf} for perfectly straight paths.
#' @export
compute_emax <- function(
  mean_cos_turning,
  mean_step_length = NULL,
  dimensional = FALSE
) {
  #--- Input validation ----------------------------------------------------
  # Values outside the feasible cosine range [-1, 1] or NA are marked invalid.
  invalid <- is.na(mean_cos_turning) |
    mean_cos_turning > 1 |
    mean_cos_turning < -1

  # Replace invalid entries with a safe placeholder (0) for the calculation.
  safe_cos <- ifelse(invalid, 0, mean_cos_turning)

  #--- Core formula (dimensionless) ---------------------------------------
  emax <- sqrt((1 + safe_cos) / (1 - safe_cos))

  # Handle the theoretical infinite case (c == 1) explicitly.
  emax[mean_cos_turning == 1] <- Inf

  #--- Convert to spatial units if requested -------------------------------
  if (dimensional) {
    if (is.null(mean_step_length)) {
      warning("`mean_step_length` is NULL; returning dimensionless Emax")
    } else {
      emax <- emax * mean_step_length
    }
  }

  # Restore NA for all originally invalid entries.
  emax[invalid] <- NA_real_
  emax
}

# Constructor and main creation functions for aniframe_kin class

#' Create a new aniframe_kin object (internal constructor)
#'
#' @param x An anipoint to add the aniframe_kin class to
#' @return An aniframe_kin object
#' @keywords internal
new_aniframe_kin <- function(x) {
  # Prepended, so it stays a child of anipoint: c("aniframe_kin", "anipoint", "aniframe", ...).
  class(x) <- c("aniframe_kin", class(x))
  class(x) <- unique(class(x))
  x
}

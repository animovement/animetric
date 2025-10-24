# Constructor and main creation functions for aniframe_kin class

#' Create a new aniframe_kin object (internal constructor)
#'
#' @param x A data frame to add aniframe_kin class to
#' @return An aniframe_kin object
#' @keywords internal
new_aniframe_kin <- function(x) {
  class(x) <- c("aniframe_kin", class(x))
  class(x) <- unique(class(x))
  x
}

#' @keywords internal
new_aniframe_kin2d <- function(x) {
  class(x) <- c("aniframe_kin2d", class(x))
  class(x) <- unique(class(x))
  x
}

#' @keywords internal
new_aniframe_kin3d <- function(x) {
  class(x) <- c("aniframe_kin3d", class(x))
  class(x) <- unique(class(x))
  x
}

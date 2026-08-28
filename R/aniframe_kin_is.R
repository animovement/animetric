#' Check if object is an aniframe_kin
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from aniframe
#' @examples
#' kin <- calculate_kinematics(
#'   anicore::example_aniframe(n_obs = 20, n_individuals = 1, n_keypoints = 1)
#' )
#' is_aniframe_kin(kin)
#'
#' # An aniframe without kinematics is not one
#' is_aniframe_kin(anicore::example_aniframe(n_obs = 3))
#' @export
is_aniframe_kin <- function(x) {
  inherits(x, "aniframe_kin")
}

#' Ensure object is an aniframe_kin
#'
#' @param x An object to test
#' @return Error if not an aniframe_kin
#' @keywords internal
ensure_is_aniframe_kin <- function(x) {
  if (!is_aniframe_kin(x)) {
    cli::cli_abort("Data is not an aniframe_kin class.")
  }
}

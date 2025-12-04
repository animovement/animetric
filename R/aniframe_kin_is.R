#' Check if object is an aniframe_kin
#'
#' @param x An object to test
#' @return Logical: TRUE if x inherits from aniframe
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

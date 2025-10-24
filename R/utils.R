#' @keywords internal
cumsum_na <- function(x) {
  cumsum(dplyr::coalesce(x, 0))
}

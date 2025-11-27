#' @keywords internal
cumsum_na <- function(x) {
  cumsum(dplyr::coalesce(x, 0))
}

#' @keywords internal
convert_nan_to_na <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(dplyr::where(is.numeric), function(x) {
      ifelse(is.nan(x), NA, x)
    })
  )
}

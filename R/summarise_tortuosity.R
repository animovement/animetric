#' Calculate tortuosity summary statistics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [summarise_path()], which returns the same measures. The name
#' suggested a summary of [add_tortuosity()]'s windowed output, which it
#' never was: it measures the whole path. To summarise the windowed
#' measures, use [summarise_aniframe()].
#'
#' This returns exactly what it did, with the columns `total_path_length` and
#' `emax`, which [summarise_path()] calls `total_distance` and `e_max`.
#'
#' @param data An anipoint.
#'
#' @return As [summarise_path()], with `total_path_length` and `emax` in
#'   place of `total_distance` and `e_max`.
#' @keywords internal
#' @export
#' @aliases summarize_tortuosity
summarise_tortuosity <- function(data) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "summarise_tortuosity()",
    "summarise_path()"
  )
  summarise_path_legacy(data)
}

#' @rdname summarise_tortuosity
#' @export
summarize_tortuosity <- summarise_tortuosity

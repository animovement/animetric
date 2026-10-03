#' Calculate tortuosity summary statistics
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [summarise_path()], which returns the same measures. The name
#' suggested a summary of [calculate_tortuosity()]'s windowed output, which it
#' never was: it measures the whole path. To summarise the windowed
#' measures, use [summarise_aniframe()].
#'
#' @param data An anipoint.
#'
#' @return As [summarise_path()].
#' @keywords internal
#' @export
#' @aliases summarize_tortuosity
summarise_tortuosity <- function(data) {
  lifecycle::deprecate_warn(
    "0.6.0",
    "summarise_tortuosity()",
    "summarise_path()"
  )
  summarise_path(data)
}

#' @rdname summarise_tortuosity
#' @export
summarize_tortuosity <- summarise_tortuosity

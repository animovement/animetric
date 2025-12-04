#' Summarise an aniframe
#'
#' Calculate summary statistics for aniframe data by dispatching to specialised
#' summary functions.
#'
#' @param data A kinematics aniframe (output of `calculate_kinematics()`)
#' @param type Character vector of summary types. Options are `"kinematics"`
#'   and `"tortuosity"`. Default is both.
#' @param measures Measures of central tendency and dispersion for kinematics.
#'   Options are `"median_mad"` (default) and `"mean_sd"`.
#'
#' @return A summarised data frame with one row per group.
#'
#' @seealso [summarise_kinematics()], [summarise_tortuosity()]
#'
#' @export
#' @aliases summarize_aniframe
summarise_aniframe <- function(
  data,
  type = c("kinematics", "tortuosity"),
  measures = c("median_mad", "mean_sd")
) {
  ensure_is_aniframe_kin(data)
  type <- match.arg(type, several.ok = TRUE)
  measures <- match.arg(measures)

  summaries <- list()

  if ("kinematics" %in% type) {
    summaries$kinematics <- summarise_kinematics(
      data,
      measures = measures
    )
  }

  if ("tortuosity" %in% type) {
    summaries$tortuosity <- summarise_tortuosity(data)
  }

  join_summaries(summaries, dplyr::group_vars(data))
}

#' @rdname summarise_aniframe
#' @export
summarize_aniframe <- summarise_aniframe

#' Join multiple summary data frames
#' @keywords internal
join_summaries <- function(summaries, group_vars) {
  if (length(summaries) == 1L) {
    return(summaries[[1L]])
  }

  if (length(group_vars) == 0L) {
    return(dplyr::bind_cols(summaries))
  }

  purrr::reduce(summaries, dplyr::left_join, by = group_vars)
}

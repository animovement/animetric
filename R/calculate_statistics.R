#' Calculate summary statistics
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Calculate summary statistics for tracks
#'
#' @param data A kinematics data frame
#' @param measures Measures of central tendency and dispersion. Options are `median_mad` (default) and `mean_sd`. See description for more information.
#' @param straightness Which method to calculate path straightness. Choose between "A" (default), "B", "C"... or a combination (e.g. "c("A","B")"). See description for details about the different calculations.
#' @return An data frame data frame with kinematics calculated
#' @export
#'
calculate_statistics <- function(
  data,
  measures = "median_mad",
  straightness = c("A", "B", "C", "D")
) {
  # Calculate translational and rotational separately (maybe?) and gather at the end
  totals <- data |>
    dplyr::summarise(
      dplyr::across(
        c("d_translation", "d_rotation"),
        ~ collapse::fsum(abs(.x)),
        .names = "total_{.col}"
      ),
      dplyr::across(
        c("x", "y"),
        ~ dplyr::last(.x, na_rm = TRUE),
        .names = "last_{.col}"
      ),
      .by = c("individual", "keypoint")
    )

  totals <- totals |>
    calculate_straightness(straightness)
  # dplyr::mutate("straightness_{{ A }} = calculate_straightness(.data$last_x, .data$last_y, .data$total_distance, method = straightness))

  if (measures == "median_mad") {
    data <- data |>
      dplyr::summarise(
        dplyr::across(
          c("direction"),
          ~ collapse::fmean(circular::circular(.x, modulo = "2pi")),
          .names = "median_{.col}"
        ),
        dplyr::across(
          c("direction"),
          ~ calculate_circular_mad(circular::circular(.x, modulo = "2pi")),
          .names = "mad_{.col}"
        ),
        dplyr::across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fmedian(abs(.x)),
          .names = "median_{.col}"
        ),
        dplyr::across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ stats::mad(abs(.x), na.rm = TRUE),
          .names = "mad_{.col}"
        ),
        .by = c("individual", "keypoint")
      ) |>
      dplyr::left_join(totals) |>
      dplyr::select(-c("last_x", "last_y")) |>
      suppressMessages()
  } else if (measures == "mean_sd") {
    data <- data |>
      dplyr::summarise(
        dplyr::across(
          c("direction"),
          ~ collapse::fmean(circular::circular(.x)),
          .names = "mean_{.col}"
        ),
        dplyr::across(
          c("direction"),
          ~ circular::sd(circular::circular(.x)),
          .names = "sd_{.col}"
        ),
        dplyr::across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fmean(abs(.x)),
          .names = "mean_{.col}"
        ),
        dplyr::across(
          c("v_translation", "a_translation", "v_rotation", "a_rotation"),
          ~ collapse::fsd(abs(.x)),
          .names = "sd_{.col}"
        ),
        .by = c("individual", "keypoint")
      ) |>
      dplyr::left_join(totals) |>
      dplyr::select(-c("last_x", "last_y")) |>
      suppressMessages()
  }

  return(data)
}

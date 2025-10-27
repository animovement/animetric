#' @export
summarise_keypoints <- function(
    data,
    keypoints = "all",
    name = "centroid",
    add_area = FALSE
    ) {
  ensure_is_aniframe(data)

  # TODO: Check whether the new centorid name already exists in the data

  if (keypoints == "all") {
    keypoints <- unique(data$keypoint)
  }
  n_keypoints <- length(keypoints)
  grps <- attr(data, "groups") |>
    names() |>
    setdiff(c(".rows", "keypoint")) |>
    c("time")

  # --------
  # DO STUFF!!!
  # --------

  if (n_keypoints == 1) {
    cli::cli_alert_info("Minimum 2 keypoints")
    return(data)
  } else if (n_keypoints == 2){

  } else if (n_keypoints > 2) {

  }

  data_polygon_pre <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grps))) |>
    dplyr::summarise(
      coords = list(
        data.frame(x = .data$x, y = .data$y)
      )
    ) |>
    dplyr::mutate(
      coords = purrr::map(coords, ~ dplyr::bind_rows(.x, .x[1, ]))
    ) |>
    suppressWarnings()

  data_polygon <- data_polygon_pre |>
    dplyr::mutate(
      # Convert to polygon (needs matrix wrapped in list)
      polygon = purrr::map(.data$coords, ~ sf::st_polygon(list(as.matrix(.x)))),
      # Calculate area
      # area = purrr::map_dbl(.data$polygon, sf::st_area),
      # Polygon centroid coordinates
      x = purrr::map_dbl(.data$coords, ~ mean(.x$x, na.rm = TRUE)),
      y = purrr::map_dbl(.data$coords, ~ mean(.x$y, na.rm = TRUE)),
      keypoint = name
    ) |>
    dplyr::select(!dplyr::all_of("coords")) |>
    aniframe::as_aniframe()

  md <- aniframe::get_metadata(data)

  data |>
    dplyr::bind_rows(data_polygon) |>
    aniframe::as_aniframe() |>
    aniframe::set_metadata(metadata = md)
}

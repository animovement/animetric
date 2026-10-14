# Tests for summarise_aniframe
#
# summarise_aniframe:
# - summarise_aniframe respects the measures argument
# - summarise_aniframe preserves grouping structure
# - summarise_aniframe works with 2D data
# - summarise_aniframe works with 3D data
# - summarise_aniframe validates input

# Helper to create mock 2D kinematics aniframe
mock_kin_2d <- function(n = 10, grouped = FALSE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  anicore::as_anipoint(data) |>
    add_kinematics()
}

# Helper to create mock 3D kinematics aniframe
mock_kin_3d <- function(n = 10, grouped = FALSE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n)),
    z = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  anicore::as_anipoint(data) |>
    add_kinematics()
}


# summarise_aniframe() on an anipoint -----------------------------------

test_that("an anipoint's default measures are the kinematics it carries", {
  result <- summarise_aniframe(mock_kin_2d())

  expect_named(
    result,
    c(
      "keypoint",
      "median_speed",
      "mad_speed",
      "median_acceleration",
      "mad_acceleration",
      "median_turning_speed",
      "mad_turning_speed",
      "median_turning_rate",
      "mad_turning_rate",
      "median_turning_acceleration",
      "mad_turning_acceleration",
      "median_course",
      "mad_course"
    )
  )
  expect_equal(nrow(result), 1L)
})

test_that("windowed tortuosity and confidence are summarised when present", {
  data <- mock_kin_2d(n = 20) |>
    dplyr::mutate(confidence = seq(0.5, 1, length.out = 20)) |>
    add_tortuosity(window_width = 5L)

  result <- summarise_aniframe(data)

  expect_true(all(
    c(
      "median_straightness_5",
      "median_sinuosity_5",
      "median_e_max_5",
      "median_confidence"
    ) %in%
      names(result)
  ))
  expect_equal(
    result$median_straightness_5,
    stats::median(data$straightness_5, na.rm = TRUE)
  )
})

test_that("every window width present is summarised, and nothing else", {
  data <- mock_kin_2d(n = 30) |>
    add_tortuosity(window_width = 5L) |>
    add_tortuosity(window_width = 11L) |>
    dplyr::mutate(straightness_index = 1, my_e_max_5 = 1)

  result <- summarise_aniframe(data)

  windowed <- c(
    "straightness_5",
    "sinuosity_5",
    "e_max_5",
    "straightness_11",
    "sinuosity_11",
    "e_max_11"
  )
  expect_true(all(paste0("median_", windowed) %in% names(result)))
  expect_false(any(grepl("straightness_index|my_e_max", names(result))))
})

test_that("unsuffixed straightness, sinuosity and emax are summarised", {
  data <- mock_kin_2d(n = 20) |>
    dplyr::mutate(straightness = 0.5, sinuosity = 0.2, emax = 3)

  result <- summarise_aniframe(data)

  expect_true(all(
    c("median_straightness", "median_sinuosity", "median_emax") %in%
      names(result)
  ))
})

test_that("components, unwrapped course and running totals are left out", {
  result <- summarise_aniframe(mock_kin_2d())

  left_out <- c(
    "v_x",
    "a_x",
    "course_unwrapped",
    "cumulative_distance",
    "cumulative_turning"
  )
  for (col in left_out) {
    expect_false(any(grepl(col, names(result), fixed = TRUE)), label = col)
  }
})

test_that("3D kinematics are summarised, with course only given a vertical", {
  without <- summarise_aniframe(mock_kin_3d())
  expect_true("median_turning_speed" %in% names(without))
  expect_false("median_course" %in% names(without))

  data <- data.frame(time = 1:20, x = cos(1:20), y = sin(1:20), z = 1:20) |>
    anicore::as_anipoint() |>
    add_kinematics(vertical = "z")
  with <- summarise_aniframe(data)
  expect_true(all(
    c("median_course", "median_course_elevation", "median_turning_rate") %in%
      names(with)
  ))
})

test_that("a declared yaw is summarised as circular heading", {
  # Facing just either side of pi: the circular median is pi, not 0
  data <- data.frame(
    time = 1:6,
    x = 1:6,
    y = 0,
    hd = c(pi - 0.1, -pi + 0.1, pi - 0.1, -pi + 0.1, pi, pi)
  ) |>
    anicore::as_anipoint() |>
    anicore::set_variables(
      where = list(position = c(x = "x", y = "y"), orientation = c(yaw = "hd"))
    )

  result <- summarise_aniframe(data, measures = "mean_sd")

  expect_equal(abs(anicore::wrap_angle(result$mean_heading, "pi")), pi)
  expect_false("mean_hd" %in% names(result))
})

test_that("measures = 'mean_sd' gives means and standard deviations", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, measures = "mean_sd")

  expect_equal(result$mean_speed, mean(data$speed, na.rm = TRUE))
  expect_equal(result$sd_speed, stats::sd(data$speed, na.rm = TRUE))
  expect_equal(
    result$mean_course,
    anicore::circ_mean(data$course)
  )
})

test_that("cols replaces the default measures, keeping known angles circular", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, cols = c("speed", "course", "x"))

  expect_named(
    result,
    c(
      "keypoint",
      "median_speed",
      "mad_speed",
      "median_x",
      "mad_x",
      "median_course",
      "mad_course"
    )
  )
  expect_equal(result$median_course, anicore::circ_median(data$course))
})

test_that("cols is checked", {
  data <- mock_kin_2d()

  expect_error(summarise_aniframe(data, cols = "nope"), "not found")
  expect_error(summarise_aniframe(data, cols = 1), "character vector")
  expect_error(summarise_aniframe(data, cols = "keypoint"), "grouping")
  expect_error(
    summarise_aniframe(dplyr::mutate(data, label = "a"), cols = "label"),
    "not numeric"
  )
  expect_error(summarise_aniframe(data, foo = 1), "must be empty")
})

test_that("an anipoint without measures says what to do", {
  plain <- anicore::as_anipoint(data.frame(time = 1:5, x = 1:5, y = 1:5))
  expect_error(summarise_aniframe(plain), "add_kinematics")
})

test_that("summaries follow any grouping, one row per group", {
  data <- mock_kin_2d(grouped = TRUE)

  per_individual <- summarise_aniframe(data)
  expect_equal(nrow(per_individual), 2L)
  expect_true("individual" %in% names(per_individual))

  # anicore warns that an ungrouped anipoint is unusual; that is the point
  pooled <- summarise_aniframe(suppressWarnings(dplyr::ungroup(data)))
  expect_equal(nrow(pooled), 1L)
  expect_equal(pooled$median_speed, stats::median(data$speed, na.rm = TRUE))
})

test_that("circular summaries are in the frame's unit_angle", {
  rad <- mock_kin_2d(n = 30)
  deg <- anicore::convert_unit_angle(
    rad,
    "deg",
    cols = c("course", "turning_rate", "turning_speed", "turning_acceleration")
  )

  expect_equal(
    summarise_aniframe(deg)$median_course,
    summarise_aniframe(rad)$median_course * 180 / pi
  )
})

# summarise_aniframe() on segments, joints and events --------------------

structured <- function() {
  anicore::example_anipoint(n_obs = 10, n_individuals = 1) |>
    anicore::set_structure(anicore::example_structure())
}

test_that("an anisegment's lengths are summarised per segment", {
  segments <- anicore::as_anisegment(structured())
  result <- summarise_aniframe(segments)

  expect_true(all(
    c("segment", "median_length", "mad_length") %in% names(result)
  ))
  expect_equal(nrow(result), dplyr::n_groups(segments))
  expect_false(any(c("median_ux", "median_uy") %in% names(result)))
})

test_that("an anijoint's angles are summarised circularly, per joint", {
  joints <- anicore::as_anijoint(structured())
  result <- summarise_aniframe(joints, measures = "mean_sd")

  expect_true(all(c("joint", "mean_angle", "sd_angle") %in% names(result)))
  first <- dplyr::filter(joints, .data$joint == result$joint[1])
  expect_equal(result$mean_angle[1], anicore::circ_mean(first$angle))
})

test_that("anievents and other objects are refused with a reason", {
  events <- anicore::anievent(
    individual = 1L,
    channel = "behaviour",
    label = "REM",
    start = 3,
    stop = 9
  )
  expect_error(summarise_aniframe(events), "#91")
  expect_error(
    summarise_aniframe(data.frame(x = 1)),
    "anipoint, anisegment or anijoint"
  )
})

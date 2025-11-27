# test-summarise-kinematics.R
# Tests for summarise_kinematics function
#
# Outline:
# --------
# Basic functionality:
#   - Returns data frame with expected columns for 2D data
#   - Returns data frame with expected columns for 3D data
#   - Errors for non-aniframe_kin input
#
# Measures argument:
#   - Default is median_mad
#   - median_mad returns median_* and mad_* columns
#   - mean_sd returns mean_* and sd_* columns
#   - Errors for invalid measures argument
#
# Tortuosity metrics:
#   - straightness = 1 for perfectly straight path
#   - straightness < 1 for curved path
#   - sinuosity is higher for more tortuous paths
#   - emax is higher for straighter paths
#   - net_displacement is correct
#   - total_path_length is correct
#
# Grouping:
#   - Respects grouping by individual
#   - Respects grouping by multiple variables
#   - Returns one row per group
#
# 2D-specific:
#   - Includes angular kinematic summaries
#   - Includes heading summaries (circular statistics)
#   - total_angular_path_length is correct
#
# 3D-specific:
#   - Does not include angular kinematic summaries
#   - Does not include heading summaries
#   - Tortuosity computed from velocity vectors
#
# Edge cases:
#   - Handles stationary points (no movement)
#   - Handles paths with NA values
#   - Handles very short paths

# =============================================================================
# Helper functions for creating test data
# =============================================================================

make_straight_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()
}

make_circular_path_2d <- function(n = 20) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()
}

make_zigzag_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq_len(n),
    y = rep(c(0, 1), length.out = n)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()
}

make_straight_path_3d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n),
    z = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()
}

make_helical_path_3d <- function(n = 20) {
  theta <- seq(0, 4 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta),
    z = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()
}

# =============================================================================
# Basic functionality
# =============================================================================

test_that("summarise_kinematics returns expected columns for 2D data", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  # Totals
  expect_true("total_path_length" %in% names(result))
  expect_true("total_angular_path_length" %in% names(result))
  expect_true("net_displacement" %in% names(result))

  # Tortuosity
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))

  # Kinematic summaries (median_mad by default)
  expect_true("median_speed" %in% names(result))
  expect_true("mad_speed" %in% names(result))
  expect_true("median_angular_speed" %in% names(result))
  expect_true("median_heading" %in% names(result))
})

test_that("summarise_kinematics returns expected columns for 3D data", {
  data <- make_straight_path_3d()
  result <- summarise_kinematics(data)

  # Totals
  expect_true("total_path_length" %in% names(result))
  expect_true("net_displacement" %in% names(result))

  # Tortuosity
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))

  # Kinematic summaries
  expect_true("median_speed" %in% names(result))
  expect_true("mad_speed" %in% names(result))

  # Should NOT have angular measures for 3D
  expect_false("median_angular_speed" %in% names(result))
  expect_false("median_heading" %in% names(result))
  expect_false("total_angular_path_length" %in% names(result))
})

test_that("summarise_kinematics errors for non-aniframe_kin input", {
  # Plain data frame
  data <- data.frame(time = 1:10, x = 1:10, y = 1:10)
  expect_error(summarise_kinematics(data))

  # aniframe without kinematics
  data <- data.frame(time = 1:10, x = 1:10, y = 1:10) |>
    aniframe::as_aniframe()
  expect_error(summarise_kinematics(data))
})

test_that("summarise_kinematics returns one row for ungrouped data", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  expect_equal(nrow(result), 1)
})

# =============================================================================
# Measures argument
# =============================================================================

test_that("summarise_kinematics uses median_mad by default", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  expect_true("median_speed" %in% names(result))
  expect_true("mad_speed" %in% names(result))
  expect_false("mean_speed" %in% names(result))
  expect_false("sd_speed" %in% names(result))
})

test_that("summarise_kinematics with mean_sd returns mean and sd columns", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data, measures = "mean_sd")

  expect_true("mean_speed" %in% names(result))
  expect_true("sd_speed" %in% names(result))
  expect_false("median_speed" %in% names(result))
  expect_false("mad_speed" %in% names(result))
})

test_that("summarise_kinematics errors for invalid measures argument", {
  data <- make_straight_path_2d()

  expect_error(
    summarise_kinematics(data, measures = "invalid")
  )
})

# =============================================================================
# Tortuosity metrics
# =============================================================================

test_that("straightness is near 1 for perfectly straight path", {
  data <- make_straight_path_2d(n = 50)
  result <- summarise_kinematics(data)

  expect_gt(result$straightness, 0.99)
})

test_that("straightness is less than 1 for curved path", {
  straight <- summarise_kinematics(make_straight_path_2d(n = 50))
  circular <- summarise_kinematics(make_circular_path_2d(n = 50))

  expect_gt(straight$straightness, circular$straightness)
  expect_lt(circular$straightness, 0.5)
})

test_that("sinuosity is higher for more tortuous paths", {
  straight <- summarise_kinematics(make_straight_path_2d(n = 50))
  zigzag <- summarise_kinematics(make_zigzag_path_2d(n = 50))

  expect_gt(zigzag$sinuosity, straight$sinuosity)
})

test_that("emax is higher for straighter paths", {
  straight <- summarise_kinematics(make_straight_path_2d(n = 50))
  zigzag <- summarise_kinematics(make_zigzag_path_2d(n = 50))

  expect_gt(straight$emax, zigzag$emax)
})

test_that("net_displacement is correct for straight path", {
  data <- make_straight_path_2d(n = 20)
  result <- summarise_kinematics(data)

  # Straight line from (0,0) to (10,10)
  expected_displacement <- sqrt(10^2 + 10^2)
  expect_equal(result$net_displacement, expected_displacement, tolerance = 0.01)
})

test_that("total_path_length is correct for straight path", {
  data <- make_straight_path_2d(n = 20)
  result <- summarise_kinematics(data)

  # For a straight path, path_length should equal displacement
  expect_equal(
    result$total_path_length,
    result$net_displacement,
    tolerance = 0.01
  )
})

test_that("total_path_length exceeds net_displacement for curved path", {
  data <- make_circular_path_2d(n = 50)
  result <- summarise_kinematics(data)

  expect_gt(result$total_path_length, result$net_displacement)
})

# =============================================================================
# Grouping
# =============================================================================

test_that("summarise_kinematics respects grouping by individual", {
  data <- dplyr::bind_rows(
    make_straight_path_2d() |> dplyr::mutate(individual = "A"),
    make_circular_path_2d() |> dplyr::mutate(individual = "B")
  ) |>
    aniframe::as_aniframe() |>
    new_aniframe_kin()

  result <- data |>
    summarise_kinematics()

  expect_equal(nrow(result), 2)
  expect_true("individual" %in% names(result))
  expect_setequal(result$individual, c("A", "B"))
})

test_that("summarise_kinematics returns different values for different groups", {
  data <- dplyr::bind_rows(
    make_straight_path_2d(n = 50) |> dplyr::mutate(individual = "straight"),
    make_zigzag_path_2d(n = 50) |> dplyr::mutate(individual = "zigzag")
  ) |>
    aniframe::as_aniframe() |>
    new_aniframe_kin() |>
    new_aniframe_kin2d()

  result <- data |>
    summarise_kinematics()

  straight_row <- result |> dplyr::filter(individual == "straight")
  zigzag_row <- result |> dplyr::filter(individual == "zigzag")

  expect_gt(straight_row$straightness, zigzag_row$straightness)
})

test_that("summarise_kinematics respects multiple grouping variables", {
  data <- dplyr::bind_rows(
    make_straight_path_2d() |>
      dplyr::mutate(individual = "A", keypoint = "nose"),
    make_straight_path_2d() |>
      dplyr::mutate(individual = "A", keypoint = "tail"),
    make_straight_path_2d() |>
      dplyr::mutate(individual = "B", keypoint = "nose")
  ) |>
    aniframe::as_aniframe() |>
    new_aniframe_kin() |>
    new_aniframe_kin2d()

  result <- data |>
    summarise_kinematics()

  expect_equal(nrow(result), 3)
  expect_true(all(c("individual", "keypoint") %in% names(result)))
})

# =============================================================================
# 2D-specific tests
# =============================================================================

test_that("2D summaries include all angular kinematics", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  angular_cols <- c(
    "median_angular_speed",
    "mad_angular_speed",
    "median_angular_velocity",
    "mad_angular_velocity",
    "median_angular_acceleration",
    "mad_angular_acceleration"
  )

  expect_true(all(angular_cols %in% names(result)))
})

test_that("2D summaries include circular heading statistics", {
  data <- make_straight_path_2d()

  result_median <- summarise_kinematics(data, measures = "median_mad")
  expect_true("median_heading" %in% names(result_median))
  expect_true("mad_heading" %in% names(result_median))

  result_mean <- summarise_kinematics(data, measures = "mean_sd")
  expect_true("mean_heading" %in% names(result_mean))
  expect_true("sd_heading" %in% names(result_mean))
})

test_that("heading summary is in valid range", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  # Heading should be in [-pi, pi] or [0, 2*pi]
  expect_true(
    (result$median_heading >= -pi && result$median_heading <= pi) ||
      (result$median_heading >= 0 && result$median_heading <= 2 * pi)
  )
})

# =============================================================================
# 3D-specific tests
# =============================================================================

test_that("3D straightness is near 1 for straight path", {
  data <- make_straight_path_3d(n = 50)
  result <- summarise_kinematics(data)

  expect_gt(result$straightness, 0.99)
})

test_that("3D straightness is lower for helical path", {
  straight <- summarise_kinematics(make_straight_path_3d(n = 50))
  helical <- summarise_kinematics(make_helical_path_3d(n = 50))

  expect_gt(straight$straightness, helical$straightness)
})

test_that("3D net_displacement is correct", {
  data <- make_straight_path_3d(n = 20)
  result <- summarise_kinematics(data)

  # Straight line from (0,0,0) to (10,10,10)
  expected_displacement <- sqrt(10^2 + 10^2 + 10^2)
  expect_equal(result$net_displacement, expected_displacement, tolerance = 0.01)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("summarise_kinematics handles stationary points", {
  stationary <- data.frame(
    time = 1:10,
    x = rep(0, 10),
    y = rep(0, 10)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()

  result <- summarise_kinematics(stationary)

  expect_equal(result$total_path_length, 0)
  expect_equal(result$net_displacement, 0)
  expect_true(is.na(result$straightness)) # 0/0
})

test_that("summarise_kinematics handles NA values in path", {
  data <- make_straight_path_2d(n = 20)
  data$x[10] <- NA

  result <- summarise_kinematics(data)

  # Should still return a result
  expect_equal(nrow(result), 1)
  expect_true("straightness" %in% names(result))
})

test_that("summarise_kinematics handles short paths", {
  short_data <- data.frame(
    time = 1:3,
    x = c(0, 1, 2),
    y = c(0, 1, 2)
  ) |>
    aniframe::as_aniframe() |>
    calculate_kinematics()

  result <- summarise_kinematics(short_data)

  expect_equal(nrow(result), 1)
  expect_true("straightness" %in% names(result))
})

test_that("internal columns are removed from output", {
  data <- make_straight_path_2d()
  result <- summarise_kinematics(data)

  internal_cols <- grep("^\\.", names(result), value = TRUE)
  expect_length(internal_cols, 0)

  # Also check for position columns used internally
  expect_false(".first_x" %in% names(result))
  expect_false(".last_x" %in% names(result))
})

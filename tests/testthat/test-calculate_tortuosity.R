# test-calculate-tortuosity.R
# Tests for tortuosity calculate functions
#
# Outline:
# --------
# calculate_tortuosity (dispatcher):
#   - Dispatches to 2D function for 2D data
#   - Dispatches to 3D function for 3D data
#   - Errors for non-Cartesian data
#   - Errors for non-aniframe input
#
# calculate_tortuosity_2d:
#   - Returns aniframe with expected columns added
#   - Computes kinematics automatically if missing
#   - Works when kinematics already present
#   - Errors when window_width < 3
#   - Respects grouping (individual, keypoint)
#   - Straightness = 1 for perfectly straight path
#   - Straightness < 1 for curved path
#   - Window width affects smoothness of results
#   - Handles NA values in input
#   - Removes internal columns (those starting with ".")
#
# calculate_tortuosity_3d:
#   - Returns aniframe with expected columns added
#   - Computes kinematics automatically if missing
#   - Works when kinematics already present
#   - Errors when window_width < 3
#   - Respects grouping
#   - Straightness = 1 for perfectly straight 3D path
#   - Handles NA values in input
#
# Edge cases:
#   - Very short paths (length < window_width)
#   - Stationary points (no movement)
#   - Single group vs multiple groups

# =============================================================================
# Helper functions for creating test data
# =============================================================================

make_straight_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe()
}

make_circular_path_2d <- function(n = 20) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta)
  ) |>
    aniframe::as_aniframe()
}

make_zigzag_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq_len(n),
    y = rep(c(0, 1), length.out = n)
  ) |>
    aniframe::as_aniframe()
}

make_straight_path_3d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n),
    z = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe()
}

make_helical_path_3d <- function(n = 20) {
  theta <- seq(0, 4 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta),
    z = seq(0, 10, length.out = n)
  ) |>
    aniframe::as_aniframe()
}

# =============================================================================
# calculate_tortuosity (dispatcher)
# =============================================================================

test_that("calculate_tortuosity dispatches to 2D function for 2D data", {
  data_2d <- make_straight_path_2d()
  result <- calculate_tortuosity(data_2d, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity dispatches to 3D function for 3D data", {
  data_3d <- make_straight_path_3d()
  result <- calculate_tortuosity(data_3d, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity errors for non-aniframe input", {
  data <- data.frame(time = 1:10, x = 1:10, y = 1:10)
  expect_error(calculate_tortuosity(data))
})

# =============================================================================
# calculate_tortuosity_2d
# =============================================================================

test_that("calculate_tortuosity_2d returns aniframe with expected columns", {
  data <- make_straight_path_2d()
  result <- calculate_tortuosity_2d(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))
})

test_that("calculate_tortuosity_2d computes kinematics automatically if missing", {
  data <- make_straight_path_2d()

  # Should not error - kinematics computed internally

  result <- calculate_tortuosity_2d(data, window_width = 5L)

  expect_true("heading" %in% names(result))
  expect_true("v_x" %in% names(result))
  expect_true("v_y" %in% names(result))
})

test_that("calculate_tortuosity_2d works when kinematics already present", {
  data <- make_straight_path_2d() |>
    calculate_kinematics()

  result <- calculate_tortuosity_2d(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity_2d errors when window_width < 3", {
  data <- make_straight_path_2d()

  expect_error(
    calculate_tortuosity_2d(data, window_width = 2L),
    "window_width"
  )
  expect_error(
    calculate_tortuosity_2d(data, window_width = 1L),
    "window_width"
  )
})

test_that("calculate_tortuosity_2d respects grouping", {
  data <- dplyr::bind_rows(
    make_straight_path_2d() |> dplyr::mutate(individual = "A"),
    make_circular_path_2d() |> dplyr::mutate(individual = "B")
  ) |>
    aniframe::as_aniframe()

  result <- data |>
    dplyr::group_by(individual) |>
    calculate_tortuosity_2d(window_width = 5L)

  # Straight path should have higher straightness than circular
  straight_mean <- result |>
    dplyr::filter(individual == "A") |>
    dplyr::pull(straightness) |>
    mean(na.rm = TRUE)

  circular_mean <- result |>
    dplyr::filter(individual == "B") |>
    dplyr::pull(straightness) |>
    mean(na.rm = TRUE)

  expect_gt(straight_mean, circular_mean)
})

test_that("calculate_tortuosity_2d gives straightness near 1 for straight path", {
  data <- make_straight_path_2d(n = 50)
  result <- calculate_tortuosity_2d(data, window_width = 11L)

  # Middle values should be very close to 1
  middle_straightness <- result$straightness[15:35]
  expect_true(all(middle_straightness > 0.99, na.rm = TRUE))
})

test_that("calculate_tortuosity_2d gives lower straightness for curved path", {
  straight <- make_straight_path_2d(n = 50) |>
    calculate_tortuosity_2d(window_width = 11L)

  circular <- make_circular_path_2d(n = 50) |>
    calculate_tortuosity_2d(window_width = 11L)

  expect_gt(
    mean(straight$straightness, na.rm = TRUE),
    mean(circular$straightness, na.rm = TRUE)
  )
})

test_that("calculate_tortuosity_2d removes internal columns", {
  data <- make_straight_path_2d()
  result <- calculate_tortuosity_2d(data, window_width = 5L)

  internal_cols <- grep("^\\.", names(result), value = TRUE)
  expect_length(internal_cols, 0)
})

test_that("calculate_tortuosity_2d handles short paths", {
  # Path shorter than window_width
  short_data <- make_straight_path_2d(n = 5)

  # Should not error
  result <- calculate_tortuosity_2d(short_data, window_width = 11L)

  expect_s3_class(result, "aniframe")
  # Will have many NAs but should still work
  expect_true("straightness" %in% names(result))
})

test_that("calculate_tortuosity_2d handles NA values in input", {
  data <- make_straight_path_2d(n = 20)
  data$x[10] <- NA

  result <- calculate_tortuosity_2d(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  # Should have NAs propagate near the missing value
  expect_true(any(is.na(result$straightness)))
})

# =============================================================================
# calculate_tortuosity_3d
# =============================================================================

test_that("calculate_tortuosity_3d returns aniframe with expected columns", {
  data <- make_straight_path_3d()
  result <- calculate_tortuosity_3d(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))
})

test_that("calculate_tortuosity_3d computes kinematics automatically if missing", {
  data <- make_straight_path_3d()

  result <- calculate_tortuosity_3d(data, window_width = 5L)

  expect_true("v_x" %in% names(result))
  expect_true("v_y" %in% names(result))
  expect_true("v_z" %in% names(result))
  expect_true("speed" %in% names(result))
})

test_that("calculate_tortuosity_3d works when kinematics already present", {
  data <- make_straight_path_3d() |>
    calculate_kinematics()

  result <- calculate_tortuosity_3d(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity_3d errors when window_width < 3", {
  data <- make_straight_path_3d()

  expect_error(
    calculate_tortuosity_3d(data, window_width = 2L),
    "window_width"
  )
})

test_that("calculate_tortuosity_3d gives straightness near 1 for straight path", {
  data <- make_straight_path_3d(n = 50)
  result <- calculate_tortuosity_3d(data, window_width = 11L)

  middle_straightness <- result$straightness[15:35]
  expect_true(all(middle_straightness > 0.99, na.rm = TRUE))
})

test_that("calculate_tortuosity_3d gives lower straightness for helical path", {
  straight <- make_straight_path_3d(n = 50) |>
    calculate_tortuosity_3d(window_width = 11L)

  helical <- make_helical_path_3d(n = 50) |>
    calculate_tortuosity_3d(window_width = 11L)

  expect_gt(
    mean(straight$straightness, na.rm = TRUE),
    mean(helical$straightness, na.rm = TRUE)
  )
})

test_that("calculate_tortuosity_3d removes internal columns", {
  data <- make_straight_path_3d()
  result <- calculate_tortuosity_3d(data, window_width = 5L)

  internal_cols <- grep("^\\.", names(result), value = TRUE)
  expect_length(internal_cols, 0)
})

test_that("calculate_tortuosity_3d respects grouping", {
  data <- dplyr::bind_rows(
    make_straight_path_3d() |> dplyr::mutate(individual = "A"),
    make_helical_path_3d() |> dplyr::mutate(individual = "B")
  ) |>
    aniframe::as_aniframe()

  result <- data |>
    dplyr::group_by(individual) |>
    calculate_tortuosity_3d(window_width = 5L)

  straight_mean <- result |>
    dplyr::filter(individual == "A") |>
    dplyr::pull(straightness) |>
    mean(na.rm = TRUE)

  helical_mean <- result |>
    dplyr::filter(individual == "B") |>
    dplyr::pull(straightness) |>
    mean(na.rm = TRUE)

  expect_gt(straight_mean, helical_mean)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("calculate_tortuosity handles stationary points", {
  # All same position
  stationary <- data.frame(
    time = 1:10,
    x = rep(0, 10),
    y = rep(0, 10)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_tortuosity_2d(stationary, window_width = 5L)

  # Path length is 0, so straightness should be NA
  expect_true(all(is.na(result$straightness)))
})

test_that("calculate_tortuosity handles minimum valid window_width", {
  data <- make_straight_path_2d(n = 10)

  # window_width = 3 is minimum valid
  result <- calculate_tortuosity_2d(data, window_width = 3L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
})

test_that("calculate_tortuosity coerces window_width to integer", {
  data <- make_straight_path_2d()

  # Should work with numeric that can be coerced
  result <- calculate_tortuosity_2d(data, window_width = 5.0)

  expect_s3_class(result, "aniframe")
})

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
# calculate_tortuosity() on 2D data:
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
# calculate_tortuosity() on 3D data:
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
#   - Class preservation through calculations

# =============================================================================
# Helper functions for creating test data
# =============================================================================

make_straight_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n)
  ) |>
    anicore::as_anipoint()
}

make_circular_path_2d <- function(n = 20) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta)
  ) |>
    anicore::as_anipoint()
}

make_zigzag_path_2d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq_len(n),
    y = rep(c(0, 1), length.out = n)
  ) |>
    anicore::as_anipoint()
}

make_straight_path_3d <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq(0, 10, length.out = n),
    y = seq(0, 10, length.out = n),
    z = seq(0, 10, length.out = n)
  ) |>
    anicore::as_anipoint()
}

make_helical_path_3d <- function(n = 20) {
  theta <- seq(0, 4 * pi, length.out = n)
  data.frame(
    time = seq_len(n),
    x = cos(theta),
    y = sin(theta),
    z = seq(0, 10, length.out = n)
  ) |>
    anicore::as_anipoint()
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
# calculate_tortuosity() on 2D data
# =============================================================================

test_that("calculate_tortuosity() on 2D data returns aniframe with expected columns", {
  data <- make_straight_path_2d()
  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))
})

test_that("calculate_tortuosity() on 2D data computes kinematics automatically if missing", {
  data <- make_straight_path_2d()

  # Should not error - kinematics computed internally

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_true("heading" %in% names(result))
  expect_true("v_x" %in% names(result))
  expect_true("v_y" %in% names(result))
})

test_that("calculate_tortuosity() on 2D data works when kinematics already present", {
  data <- make_straight_path_2d() |>
    calculate_kinematics()

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity() on 2D data errors when window_width < 3", {
  data <- make_straight_path_2d()

  expect_error(
    calculate_tortuosity(data, window_width = 2L),
    "window_width"
  )
  expect_error(
    calculate_tortuosity(data, window_width = 1L),
    "window_width"
  )
})

test_that("calculate_tortuosity() on 2D data respects grouping", {
  data <- dplyr::bind_rows(
    make_straight_path_2d() |> dplyr::mutate(individual = "A"),
    make_circular_path_2d() |> dplyr::mutate(individual = "B")
  ) |>
    anicore::as_anipoint()

  # An anipoint is grouped by its declared keys, one trajectory per group
  result <- calculate_tortuosity(data, window_width = 5L)

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

test_that("calculate_tortuosity() on 2D data gives straightness near 1 for straight path", {
  data <- make_straight_path_2d(n = 50)
  result <- calculate_tortuosity(data, window_width = 11L)

  # Middle values should be very close to 1
  middle_straightness <- result$straightness[15:35]
  expect_true(all(middle_straightness > 0.99, na.rm = TRUE))
})

test_that("calculate_tortuosity() on 2D data gives lower straightness for curved path", {
  straight <- make_straight_path_2d(n = 50) |>
    calculate_tortuosity(window_width = 11L)

  circular <- make_circular_path_2d(n = 50) |>
    calculate_tortuosity(window_width = 11L)

  expect_gt(
    mean(straight$straightness, na.rm = TRUE),
    mean(circular$straightness, na.rm = TRUE)
  )
})

test_that("calculate_tortuosity() on 2D data removes internal columns", {
  data <- make_straight_path_2d()
  result <- calculate_tortuosity(data, window_width = 5L)

  internal_cols <- grep("^\\.", names(result), value = TRUE)
  expect_length(internal_cols, 0)
})

test_that("calculate_tortuosity() on 2D data handles short paths", {
  # Path shorter than window_width
  short_data <- make_straight_path_2d(n = 5)

  # Should not error
  result <- calculate_tortuosity(short_data, window_width = 11L)

  expect_s3_class(result, "aniframe")
  # Will have many NAs but should still work
  expect_true("straightness" %in% names(result))
})

test_that("calculate_tortuosity() on 2D data handles NA values in input", {
  data <- make_straight_path_2d(n = 20)
  data$x[10] <- NA

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  # Should have NAs propagate near the missing value
  expect_true(any(is.na(result$straightness)))
})

# =============================================================================
# calculate_tortuosity() on 3D data
# =============================================================================

test_that("calculate_tortuosity() on 3D data returns aniframe with expected columns", {
  data <- make_straight_path_3d()
  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
  expect_true("sinuosity" %in% names(result))
  expect_true("emax" %in% names(result))
})

test_that("calculate_tortuosity() on 3D data computes kinematics automatically if missing", {
  data <- make_straight_path_3d()

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_true("v_x" %in% names(result))
  expect_true("v_y" %in% names(result))
  expect_true("v_z" %in% names(result))
  expect_true("speed" %in% names(result))
})

test_that("calculate_tortuosity() on 3D data works when kinematics already present", {
  data <- make_straight_path_3d() |>
    calculate_kinematics()

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "aniframe")
  expect_true(all(c("straightness", "sinuosity", "emax") %in% names(result)))
})

test_that("calculate_tortuosity() on 3D data errors when window_width < 3", {
  data <- make_straight_path_3d()

  expect_error(
    calculate_tortuosity(data, window_width = 2L),
    "window_width"
  )
})

test_that("calculate_tortuosity() on 3D data gives straightness near 1 for straight path", {
  data <- make_straight_path_3d(n = 50)
  result <- calculate_tortuosity(data, window_width = 11L)

  middle_straightness <- result$straightness[15:35]
  expect_true(all(middle_straightness > 0.99, na.rm = TRUE))
})

test_that("calculate_tortuosity() on 3D data gives lower straightness for helical path", {
  straight <- make_straight_path_3d(n = 50) |>
    calculate_tortuosity(window_width = 11L)

  helical <- make_helical_path_3d(n = 50) |>
    calculate_tortuosity(window_width = 11L)

  expect_gt(
    mean(straight$straightness, na.rm = TRUE),
    mean(helical$straightness, na.rm = TRUE)
  )
})

test_that("calculate_tortuosity() on 3D data removes internal columns", {
  data <- make_straight_path_3d()
  result <- calculate_tortuosity(data, window_width = 5L)

  internal_cols <- grep("^\\.", names(result), value = TRUE)
  expect_length(internal_cols, 0)
})

test_that("calculate_tortuosity() on 3D data respects grouping", {
  data <- dplyr::bind_rows(
    make_straight_path_3d() |> dplyr::mutate(individual = "A"),
    make_helical_path_3d() |> dplyr::mutate(individual = "B")
  ) |>
    anicore::as_anipoint()

  # An anipoint is grouped by its declared keys, one trajectory per group
  result <- calculate_tortuosity(data, window_width = 5L)

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
    anicore::as_anipoint()

  result <- calculate_tortuosity(stationary, window_width = 5L)

  # Path length is 0, so straightness should be NA
  expect_true(all(is.na(result$straightness)))
})

test_that("calculate_tortuosity handles minimum valid window_width", {
  data <- make_straight_path_2d(n = 10)

  # window_width = 3 is minimum valid
  result <- calculate_tortuosity(data, window_width = 3L)

  expect_s3_class(result, "aniframe")
  expect_true("straightness" %in% names(result))
})

test_that("calculate_tortuosity coerces window_width to integer", {
  data <- make_straight_path_2d()

  # Should work with numeric that can be coerced
  result <- calculate_tortuosity(data, window_width = 5.0)

  expect_s3_class(result, "aniframe")
})

test_that("calculate_tortuosity preserves incoming class", {
  # Create a simple aniframe with a custom subclass

  data <- data.frame(
    time = 1:20,
    x = cumsum(rnorm(20)),
    y = cumsum(rnorm(20))
  ) |>
    anicore::as_anipoint() |>
    calculate_kinematics()

  # Add a custom subclass
  class(data) <- c("custom_aniframe", class(data))

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_s3_class(result, "custom_aniframe")
  expect_s3_class(result, "aniframe_kin")
  expect_s3_class(result, "aniframe")
})

# =============================================================================
# Any number of axes, any column names (#81)
# =============================================================================

column_values <- function(data, cols) {
  lapply(rlang::set_names(cols), \(col) data[[col]])
}

test_that("calculate_tortuosity() reads renamed axis columns from the frame", {
  d <- data.frame(
    time = 1:20,
    x = cos(seq(0, 2 * pi, length.out = 20)),
    y = sin(seq(0, 2 * pi, length.out = 20))
  )
  standard <- anicore::as_anipoint(d)
  renamed <- anicore::as_anipoint(
    dplyr::rename(d, u = "x", v = "y"),
    variables_where = c(x = "u", y = "v")
  )

  metrics <- c("straightness", "sinuosity", "emax")
  expect_equal(
    column_values(calculate_tortuosity(renamed, window_width = 5L), metrics),
    column_values(calculate_tortuosity(standard, window_width = 5L), metrics)
  )
})

test_that("calculate_tortuosity() works on 1D data", {
  # Out and back: straight within each leg, a reversal at the turn
  data <- anicore::as_anipoint(data.frame(time = 1:21, x = c(0:10, 9:0)))

  result <- calculate_tortuosity(data, window_width = 5L)

  expect_equal(result$straightness[5], 1)
  expect_lt(result$straightness[11], 1)
})

test_that("the turning angle is the same in 2D and 3D for a planar path", {
  theta <- seq(0, 2 * pi, length.out = 30)
  d <- data.frame(time = seq_along(theta), x = cos(theta), y = sin(theta))
  flat_3d <- anicore::as_anipoint(transform(d, z = 0))

  metrics <- c("straightness", "sinuosity", "emax")
  expect_equal(
    column_values(calculate_tortuosity(flat_3d, window_width = 7L), metrics),
    column_values(
      calculate_tortuosity(anicore::as_anipoint(d), window_width = 7L),
      metrics
    )
  )
})

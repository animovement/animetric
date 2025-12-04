# Tests for summarise_kinematics
#
# - summarise_kinematics returns correct columns for 2D data
# - summarise_kinematics returns correct columns for 3D data
# - summarise_kinematics respects measures argument (median_mad vs mean_sd)
# - summarise_kinematics preserves grouping structure
# - summarise_kinematics validates input with .check = TRUE
# - summarise_kinematics_2d computes median/mad correctly
# - summarise_kinematics_2d computes mean/sd correctly
# - summarise_kinematics_2d includes circular statistics for heading
# - summarise_kinematics_3d computes median/mad correctly
# - summarise_kinematics_3d computes mean/sd correctly
# - summarise_kinematics_3d excludes angular columns

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

  aniframe::as_aniframe(data) |>
    calculate_kinematics()
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

  aniframe::as_aniframe(data) |>
    calculate_kinematics()
}


# summarise_kinematics: 2D output columns --------------------------------

test_that("summarise_kinematics returns correct columns for 2D median_mad", {
  data <- mock_kin_2d()
  result <- summarise_kinematics(data, measures = "median_mad")

  expected_cols <- c(
    "median_speed",
    "mad_speed",
    "median_acceleration",
    "mad_acceleration",
    "median_angular_speed",
    "mad_angular_speed",
    "median_angular_velocity",
    "mad_angular_velocity",
    "median_angular_acceleration",
    "mad_angular_acceleration",
    "median_heading",
    "mad_heading"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_kinematics returns correct columns for 2D mean_sd", {
  data <- mock_kin_2d()
  result <- summarise_kinematics(data, measures = "mean_sd")

  expected_cols <- c(
    "mean_speed",
    "sd_speed",
    "mean_acceleration",
    "sd_acceleration",
    "mean_angular_speed",
    "sd_angular_speed",
    "mean_angular_velocity",
    "sd_angular_velocity",
    "mean_angular_acceleration",
    "sd_angular_acceleration",
    "mean_heading",
    "sd_heading"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_kinematics: 3D output columns --------------------------------

test_that("summarise_kinematics returns correct columns for 3D median_mad", {
  data <- mock_kin_3d()
  result <- summarise_kinematics(data, measures = "median_mad")

  expected_cols <- c(
    "median_speed",
    "mad_speed",
    "median_acceleration",
    "mad_acceleration"
  )
  excluded_cols <- c("median_heading", "median_angular_speed")

  expect_true(all(expected_cols %in% names(result)))
  expect_false(any(excluded_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_kinematics returns correct columns for 3D mean_sd", {
  data <- mock_kin_3d()
  result <- summarise_kinematics(data, measures = "mean_sd")

  expected_cols <- c(
    "mean_speed",
    "sd_speed",
    "mean_acceleration",
    "sd_acceleration"
  )
  excluded_cols <- c("mean_heading", "mean_angular_speed")

  expect_true(all(expected_cols %in% names(result)))
  expect_false(any(excluded_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_kinematics: grouping -----------------------------------------

test_that("summarise_kinematics preserves grouping structure", {
  data <- mock_kin_2d(grouped = TRUE)
  result <- summarise_kinematics(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})

test_that("summarise_kinematics works with 3D grouped data", {
  data <- mock_kin_3d(grouped = TRUE)
  result <- summarise_kinematics(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})


# summarise_kinematics: computation correctness --------------------------

test_that("summarise_kinematics_2d computes median/mad correctly", {
  data <- mock_kin_2d()
  result <- summarise_kinematics_2d(data, measures = "median_mad")

  expect_equal(result$median_speed, median(data$speed, na.rm = TRUE))
  expect_equal(result$mad_speed, mad(data$speed, na.rm = TRUE))
  expect_equal(
    result$median_acceleration,
    median(data$acceleration, na.rm = TRUE)
  )
})

test_that("summarise_kinematics_2d computes mean/sd correctly", {
  data <- mock_kin_2d()
  result <- summarise_kinematics_2d(data, measures = "mean_sd")

  expect_equal(result$mean_speed, mean(data$speed, na.rm = TRUE))
  expect_equal(result$sd_speed, sd(data$speed, na.rm = TRUE))
  expect_equal(result$mean_acceleration, mean(data$acceleration, na.rm = TRUE))
})

test_that("summarise_kinematics_3d computes median/mad correctly", {
  data <- mock_kin_3d()
  result <- summarise_kinematics_3d(data, measures = "median_mad")

  expect_equal(result$median_speed, median(data$speed, na.rm = TRUE))
  expect_equal(result$mad_speed, mad(data$speed, na.rm = TRUE))
})

test_that("summarise_kinematics_3d computes mean/sd correctly", {
  data <- mock_kin_3d()
  result <- summarise_kinematics_3d(data, measures = "mean_sd")

  expect_equal(result$mean_speed, mean(data$speed, na.rm = TRUE))
  expect_equal(result$sd_speed, sd(data$speed, na.rm = TRUE))
})


# summarise_kinematics: circular statistics ------------------------------
test_that("summarise_kinematics_2d uses circular statistics for heading", {
  # Create data with known heading values
  data <- mock_kin_2d()
  data$heading <- rep(c(-pi + 0.1, pi - 0.1), length.out = nrow(data))
  data <- aniframe::as_aniframe(data)

  result_median <- summarise_kinematics_2d(data, measures = "median_mad")
  result_mean <- summarise_kinematics_2d(data, measures = "mean_sd")

  # Circular median/mean of values near +/- pi should be near pi, not near 0

  expect_true(abs(result_median$median_heading) > 2)
  expect_true(abs(result_mean$mean_heading) > 2)
})

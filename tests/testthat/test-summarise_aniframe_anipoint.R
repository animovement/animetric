# Tests for summarise_kinematics
#
# - summarise_aniframe returns correct columns for 2D data
# - summarise_aniframe returns correct columns for 3D data
# - summarise_aniframe respects measures argument (median_mad vs mean_sd)
# - summarise_aniframe preserves grouping structure
# - summarise_aniframe validates input
# - summarise_aniframe_2d computes median/mad correctly
# - summarise_aniframe_2d computes mean/sd correctly
# - summarise_aniframe_2d includes circular statistics for course
# - summarise_aniframe_3d computes median/mad correctly
# - summarise_aniframe_3d computes mean/sd correctly
# - summarise_aniframe_3d excludes angular columns

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


# summarise_aniframe: 2D output columns --------------------------------

test_that("summarise_aniframe returns correct columns for 2D median_mad", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, measures = "median_mad")

  expected_cols <- c(
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

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_aniframe returns correct columns for 2D mean_sd", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, measures = "mean_sd")

  expected_cols <- c(
    "mean_speed",
    "sd_speed",
    "mean_acceleration",
    "sd_acceleration",
    "mean_turning_speed",
    "sd_turning_speed",
    "mean_turning_rate",
    "sd_turning_rate",
    "mean_turning_acceleration",
    "sd_turning_acceleration",
    "mean_course",
    "sd_course"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_aniframe: 3D output columns --------------------------------

test_that("summarise_aniframe returns correct columns for 3D median_mad", {
  data <- mock_kin_3d()
  result <- summarise_aniframe(data, measures = "median_mad")

  expected_cols <- c(
    "median_speed",
    "mad_speed",
    "median_acceleration",
    "mad_acceleration",
    "median_turning_speed",
    "mad_turning_speed"
  )
  # No vertical given, so no course and no signed turning rate
  excluded_cols <- c("median_course", "median_turning_rate")

  expect_true(all(expected_cols %in% names(result)))
  expect_false(any(excluded_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_aniframe returns correct columns for 3D mean_sd", {
  data <- mock_kin_3d()
  result <- summarise_aniframe(data, measures = "mean_sd")

  expected_cols <- c(
    "mean_speed",
    "sd_speed",
    "mean_acceleration",
    "sd_acceleration",
    "mean_turning_speed",
    "sd_turning_speed"
  )
  excluded_cols <- c("mean_course", "mean_turning_rate")

  expect_true(all(expected_cols %in% names(result)))
  expect_false(any(excluded_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_aniframe: grouping -----------------------------------------

test_that("summarise_aniframe preserves grouping structure", {
  data <- mock_kin_2d(grouped = TRUE)
  result <- summarise_aniframe(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})

test_that("summarise_aniframe works with 3D grouped data", {
  data <- mock_kin_3d(grouped = TRUE)
  result <- summarise_aniframe(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})


# summarise_aniframe: computation correctness --------------------------

test_that("summarise_aniframe() on 2D data computes median/mad correctly", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, measures = "median_mad")

  expect_equal(result$median_speed, median(data$speed, na.rm = TRUE))
  expect_equal(result$mad_speed, mad(data$speed, na.rm = TRUE))
  expect_equal(
    result$median_acceleration,
    median(data$acceleration, na.rm = TRUE)
  )
})

test_that("summarise_aniframe() on 2D data computes mean/sd correctly", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, measures = "mean_sd")

  expect_equal(result$mean_speed, mean(data$speed, na.rm = TRUE))
  expect_equal(result$sd_speed, sd(data$speed, na.rm = TRUE))
  expect_equal(result$mean_acceleration, mean(data$acceleration, na.rm = TRUE))
})

test_that("summarise_aniframe() on 3D data computes median/mad correctly", {
  data <- mock_kin_3d()
  result <- summarise_aniframe(data, measures = "median_mad")

  expect_equal(result$median_speed, median(data$speed, na.rm = TRUE))
  expect_equal(result$mad_speed, mad(data$speed, na.rm = TRUE))
})

test_that("summarise_aniframe() on 3D data computes mean/sd correctly", {
  data <- mock_kin_3d()
  result <- summarise_aniframe(data, measures = "mean_sd")

  expect_equal(result$mean_speed, mean(data$speed, na.rm = TRUE))
  expect_equal(result$sd_speed, sd(data$speed, na.rm = TRUE))
})


# summarise_aniframe: circular statistics ------------------------------
test_that("summarise_aniframe() on 2D data uses circular statistics for course", {
  # Create data with known course values
  data <- mock_kin_2d()
  data$course <- rep(c(-pi + 0.1, pi - 0.1), length.out = nrow(data))
  data <- anicore::as_anipoint(data)

  result_median <- summarise_aniframe(data, measures = "median_mad")
  result_mean <- summarise_aniframe(data, measures = "mean_sd")

  # Circular median/mean of values near +/- pi should be near pi, not near 0

  expect_true(abs(result_median$median_course) > 2)
  expect_true(abs(result_mean$mean_course) > 2)
})

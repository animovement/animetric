# Tests for compute_centroid()
# - Basic centroid calculation
# - Include specific keypoints
# - Exclude specific keypoints
# - Cannot use both include and exclude
# - Custom centroid name
# - Handles NA values correctly
# - Handles missing z coordinate
# - Works with 3D data
# - NaN values converted to NA
# - Non-aniframe input errors
# - Non-Cartesian data errors

test_that("compute_centroid calculates basic centroid correctly", {
  data <- data.frame(
    time = rep(1:3, each = 2),
    keypoint = rep(c("point1", "point2"), 3),
    x = c(0, 2, 1, 3, 2, 4),
    y = c(0, 2, 1, 3, 2, 4),
    confidence = rep(0.9, 6)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_s3_class(result, "aniframe")
  expect_equal(nrow(result), 3)
  expect_equal(as.character(unique(result$keypoint)), "centroid")
  expect_equal(result$x, c(1, 2, 3))
  expect_equal(result$y, c(1, 2, 3))
  expect_true(all(is.na(result$confidence)))
})

test_that("compute_centroid works with include_keypoints", {
  data <- data.frame(
    time = rep(1, 3),
    keypoint = c("point1", "point2", "point3"),
    x = c(0, 3, 6),
    y = c(0, 3, 6),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- compute_centroid(data, include_keypoints = c("point1", "point2"))

  expect_equal(result$x, 1.5)
  expect_equal(result$y, 1.5)
})

test_that("compute_centroid works with exclude_keypoints", {
  data <- data.frame(
    time = rep(1, 3),
    keypoint = c("point1", "point2", "point3"),
    x = c(0, 3, 6),
    y = c(0, 3, 6),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- compute_centroid(data, exclude_keypoints = "point3")

  expect_equal(result$x, 1.5)
  expect_equal(result$y, 1.5)
})

test_that("compute_centroid errors when both include and exclude specified", {
  data <- data.frame(
    time = 1,
    keypoint = "point1",
    x = 1,
    y = 1,
    confidence = 0.9
  ) |>
    as_aniframe()

  expect_error(
    compute_centroid(
      data,
      include_keypoints = "point1",
      exclude_keypoints = "point2"
    ),
    "Cannot specify both"
  )
})

test_that("compute_centroid uses custom centroid name", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- compute_centroid(data, centroid_name = "center")

  expect_equal(as.character(unique(result$keypoint)), "center")
})

test_that("compute_centroid handles NA values correctly", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2", "point3"),
    x = c(0, NA, 4),
    y = c(0, 2, 4),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_equal(result$x, 2)
  expect_equal(result$y, 2)
})

test_that("compute_centroid handles all NA values", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(NA_real_, NA_real_),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_true(is.na(result$x))
  expect_equal(result$y, 1)
})

test_that("compute_centroid works without z coordinate", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_false("z" %in% names(result))
})

test_that("compute_centroid works with 3D data", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    z = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_true("z" %in% names(result))
  expect_equal(result$z, 1)
})

test_that("compute_centroid converts NaN to NA", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(Inf, -Inf),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_true(is.na(result$x))
})

test_that("compute_centroid errors on non-aniframe input", {
  data <- data.frame(
    time = 1,
    keypoint = "point1",
    x = 1,
    y = 1
  )

  expect_error(compute_centroid(data))
})

test_that("compute_centroid errors on non-Cartesian data", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(Inf, -Inf),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    aniframe::as_aniframe() |>
    anispace::map_to_polar()

  expect_error(compute_centroid(data), "Cartesian coordinate system")
})

test_that("compute_centroid preserves grouping variables", {
  data <- data.frame(
    time = rep(1:2, each = 4),
    individual = rep(c("ind1", "ind2"), each = 2, times = 2),
    keypoint = rep(c("point1", "point2"), 4),
    x = 1:8,
    y = 1:8,
    confidence = rep(0.9, 8)
  ) |>
    as_aniframe()

  result <- compute_centroid(data)

  expect_equal(nrow(result), 4)
  expect_true("individual" %in% names(result))
  expect_equal(sort(unique(result$individual)), factor(c("ind1", "ind2")))
})

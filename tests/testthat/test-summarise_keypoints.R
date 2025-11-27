# Tests for summarise_keypoints()
# - Basic functionality adds centroid
# - Uses all keypoints by default
# - Works with specific keypoints
# - Custom summary name
# - Name conflict with existing keypoint errors
# - Less than 2 keypoints errors
# - Exactly 2 keypoints works
# - More than 2 keypoints works
# - Metadata is preserved
# - Non-aniframe input errors

test_that("summarise_keypoints adds centroid to data", {
  data <- data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("point1", "point2"), 2),
    x = c(0, 2, 1, 3),
    y = c(0, 2, 1, 3),
    confidence = rep(0.9, 4)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  expect_s3_class(result, "aniframe")
  expect_equal(nrow(result), 6)
  expect_true("centroid" %in% result$keypoint)
  expect_equal(length(unique(result$keypoint)), 3)
})

test_that("summarise_keypoints uses all keypoints by default", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2", "point3"),
    x = c(0, 3, 6),
    y = c(0, 3, 6),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  centroid <- result |>
    dplyr::filter(keypoint == "centroid")

  expect_equal(centroid$x, 3)
  expect_equal(centroid$y, 3)
})

test_that("summarise_keypoints works with specific keypoints", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2", "point3"),
    x = c(0, 2, 6),
    y = c(0, 2, 6),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data, keypoints = c("point1", "point2"))

  centroid <- result |>
    dplyr::filter(keypoint == "centroid")

  expect_equal(centroid$x, 1)
  expect_equal(centroid$y, 1)
})

test_that("summarise_keypoints uses custom summary name", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data, name = "center")

  expect_true("center" %in% result$keypoint)
  expect_false("centroid" %in% result$keypoint)
})

test_that("summarise_keypoints errors on name conflict", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  expect_error(
    summarise_keypoints(data, name = "point1"),
    "conflicts with existing keypoint"
  )
})

test_that("summarise_keypoints errors with less than 2 keypoints", {
  data <- data.frame(
    time = 1,
    keypoint = "point1",
    x = 1,
    y = 1,
    confidence = 0.9
  ) |>
    as_aniframe()

  expect_error(
    summarise_keypoints(data),
    "At least 2 keypoints required"
  )
})

test_that("summarise_keypoints errors with single specified keypoint", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  expect_error(
    summarise_keypoints(data, keypoints = "point1"),
    "At least 2 keypoints required"
  )
})

test_that("summarise_keypoints works with exactly 2 keypoints", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  expect_equal(nrow(result), 3)
  expect_true("centroid" %in% result$keypoint)
})

test_that("summarise_keypoints works with more than 2 keypoints", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2", "point3", "point4"),
    x = c(0, 2, 4, 6),
    y = c(0, 2, 4, 6),
    confidence = rep(0.9, 4)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  expect_equal(nrow(result), 5)
  expect_true("centroid" %in% result$keypoint)
})

test_that("summarise_keypoints preserves metadata", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2),
    confidence = c(0.9, 0.9)
  ) |>
    as_aniframe()

  metadata <- list(sampling_rate = 30)
  data <- aniframe::set_metadata(data, metadata = metadata)

  result <- summarise_keypoints(data)

  expect_equal(
    aniframe::get_metadata(result)$sampling_rate,
    metadata$sampling_rate
  )
})

test_that("summarise_keypoints errors on non-aniframe input", {
  data <- data.frame(
    time = 1,
    keypoint = c("point1", "point2"),
    x = c(0, 2),
    y = c(0, 2)
  )

  expect_error(summarise_keypoints(data))
})

test_that("summarise_keypoints preserves original data", {
  data <- data.frame(
    time = rep(1, 3),
    keypoint = c("point1", "point2", "point3"),
    x = c(0, 2, 4),
    y = c(0, 2, 4),
    confidence = rep(0.9, 3)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  original_rows <- result |>
    dplyr::filter(keypoint != "centroid")

  expect_equal(nrow(original_rows), 3)
  expect_equal(original_rows$x, c(0, 2, 4))
})

test_that("summarise_keypoints works across multiple time points", {
  data <- data.frame(
    time = rep(1:3, each = 2),
    keypoint = rep(c("point1", "point2"), 3),
    x = 1:6,
    y = 1:6,
    confidence = rep(0.9, 6)
  ) |>
    as_aniframe()

  result <- summarise_keypoints(data)

  centroids <- result |>
    dplyr::filter(keypoint == "centroid")

  expect_equal(nrow(centroids), 3)
  expect_equal(centroids$x, c(1.5, 3.5, 5.5))
})

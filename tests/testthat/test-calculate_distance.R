# tests/testthat/test-calculate-distance.R
#
# Tests planned:
# 1. Correct Euclidean distance for simple (dx, dy) pairs
# 2. Vectorised input works element‑wise
# 3. Handles zero distances (dx = 0 or dy = 0)
# 4. Propagates NA when either component is NA
# 5. Matches manual sqrt(dx^2 + dy^2) calculation for random data

test_that("calculate_distance returns correct Euclidean distance", {
  expect_equal(calculate_distance(3, 4), 5) # 3‑4‑5 triangle
  expect_equal(calculate_distance(0, 0), 0) # origin
  expect_equal(calculate_distance(-3, 4), 5) # sign of dx ignored by squaring
  expect_equal(calculate_distance(5, -12), 13) # 5‑12‑13 triangle
})

test_that("vectorised input produces vectorised output", {
  dx <- c(3, 0, -5, 7)
  dy <- c(4, 2, 12, -24)
  expected <- sqrt(dx^2 + dy^2)
  expect_equal(calculate_distance(dx, dy), expected)
})

test_that("zero components are handled correctly", {
  expect_equal(calculate_distance(0, 5), 5)
  expect_equal(calculate_distance(7, 0), 7)
  expect_equal(calculate_distance(0, 0), 0)
})

test_that("NA values propagate to NA result", {
  expect_true(is.na(calculate_distance(NA_real_, 1)))
  expect_true(is.na(calculate_distance(1, NA_real_)))

  # Mixed vector with NAs
  dx <- c(3, NA, 0)
  dy <- c(4, 5, NA)
  res <- calculate_distance(dx, dy)
  expect_true(is.na(res[2])) # NA in dx
  expect_true(is.na(res[3])) # NA in dy
  expect_equal(res[1], sqrt(3^2 + 4^2))
})

test_that("matches manual computation for random data", {
  set.seed(42)
  dx <- runif(30, -20, 20)
  dy <- runif(30, -20, 20)

  # Remove any (dx,dy) that are both NA – none here, but keep for safety
  expected <- sqrt(dx^2 + dy^2)
  expect_equal(calculate_distance(dx, dy), expected)
})

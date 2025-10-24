# tests/testthat/test-calculate-direction.R
#
# Tests planned:
# 1. Correct angle for basic quadrants (dx, dy) pairs
# 2. Handles the (0,0) case → returns NA
# 3. Works with vectorised input (multiple dx/dy values)
# 4. Preserves NA propagation when either component is NA
# 5. Consistency with base::atan2 (numerical equality)

test_that("calculate_direction returns correct quadrant angles", {
  # Quadrant I
  expect_equal(calculate_direction(1, 1), atan2(1, 1))
  # Quadrant II
  expect_equal(calculate_direction(-1, 1), atan2(1, -1))
  # Quadrant III
  expect_equal(calculate_direction(-1, -1), atan2(-1, -1))
  # Quadrant IV
  expect_equal(calculate_direction(1, -1), atan2(-1, 1))
})

test_that("origin (0,0) yields NA", {
  expect_true(is.na(calculate_direction(0, 0)))
})

test_that("vectorised input produces vectorised output", {
  dx <- c(1, -1, 0, 0)
  dy <- c(0, 0, 1, -1)
  expected <- c(atan2(0, 1), atan2(0, -1), atan2(1, 0), atan2(-1, 0))
  # The third and fourth elements are not (0,0) so they should be valid angles
  expect_equal(calculate_direction(dx, dy), expected)
})

test_that("NA components propagate to NA result", {
  expect_true(is.na(calculate_direction(NA_real_, 1)))
  expect_true(is.na(calculate_direction(1, NA_real_)))
  # Mixed vector with NAs
  dx <- c(1, NA, 0)
  dy <- c(0, 1, NA)
  res <- calculate_direction(dx, dy)
  expect_true(is.na(res[2])) # NA in dx
  expect_true(is.na(res[3])) # NA in dy
  expect_equal(res[1], atan2(0, 1))
})

test_that("output matches base::atan2 for non‑origin points", {
  set.seed(123)
  dx <- runif(20, -10, 10)
  dy <- runif(20, -10, 10)
  # Avoid (0,0) which is handled specially
  idx <- !(dx == 0 & dy == 0)
  expect_equal(
    calculate_direction(dx[idx], dy[idx]),
    atan2(dy[idx], dx[idx])
  )
})

# Tests for mean_angle():
# - Returns value in [0, 2*pi)
# - Handles angles already in range
# - Handles angles outside [0, 2*pi)
# - Single angle input
# - Consistent with circular statistics
#
# Tests for median_angle():
# - Returns value in [0, 2*pi)
# - Handles angles already in range
# - Handles angles outside [0, 2*pi)
# - Single angle input
# - Odd and even length vectors

test_that("mean_angle returns values in [0, 2*pi)", {
  result <- mean_angle(c(0, pi / 2, pi, 3 * pi / 2))
  expect_gte(result, 0)
  expect_lt(result, 2 * pi)
})

test_that("mean_angle handles angles outside [0, 2*pi)", {
  result1 <- mean_angle(c(-pi / 2, pi / 2))
  result2 <- mean_angle(c(3 * pi / 2, pi / 2))
  expect_equal(result1, result2)

  result3 <- mean_angle(c(0, 4 * pi))
  result4 <- mean_angle(c(0, 0))
  expect_equal(result3, result4)
})

test_that("mean_angle handles single angle", {
  angle <- pi / 4
  result <- mean_angle(angle)
  expect_equal(result, angle)
})

test_that("mean_angle is consistent", {
  angles <- seq(0, 2 * pi, length.out = 7)[-7]
  result <- mean_angle(angles)
  expect_gte(result, 0)
  expect_lt(result, 2 * pi)
})

test_that("mean_angle handles clustered angles", {
  angles <- c(0.1, 0.15, 0.2)
  result <- mean_angle(angles)
  expect_gt(result, 0)
  expect_lt(result, 0.3)
})

test_that("median_angle returns values in [0, 2*pi)", {
  result <- median_angle(c(0, pi / 2, pi, 3 * pi / 2))
  expect_gte(result, 0)
  expect_lt(result, 2 * pi)
})

test_that("median_angle handles angles outside [0, 2*pi)", {
  result1 <- median_angle(c(-pi / 2, pi / 2))
  result2 <- median_angle(c(3 * pi / 2, pi / 2))
  expect_equal(result1, result2)

  result3 <- median_angle(c(0, 4 * pi))
  result4 <- median_angle(c(0, 0))
  expect_equal(result3, result4)
})

test_that("median_angle handles single angle", {
  angle <- pi / 4
  result <- median_angle(angle)
  expect_equal(result, angle)
})

test_that("median_angle handles odd and even length vectors", {
  odd_vec <- c(0, pi / 4, pi / 2)
  even_vec <- c(0, pi / 4, pi / 2, 3 * pi / 4)

  result_odd <- median_angle(odd_vec)
  result_even <- median_angle(even_vec)

  expect_gte(result_odd, 0)
  expect_lt(result_odd, 2 * pi)
  expect_gte(result_even, 0)
  expect_lt(result_even, 2 * pi)
})

test_that("median_angle handles clustered angles", {
  angles <- c(0.1, 0.15, 0.2)
  result <- median_angle(angles)
  expect_gt(result, 0)
  expect_lt(result, 0.3)
})

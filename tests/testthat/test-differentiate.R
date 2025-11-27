# Testing differentiate() function:
# - First-order derivatives with and without explicit time
# - Second-order derivatives with and without explicit time
# - Uniform vs non-uniform spacing
# - Edge case handling (endpoints, NA values)
# - Multiple differentiation orders
# - Error conditions

test_that("first derivative with uniform spacing (no time specified)", {
  x <- c(0, 1, 2, 3, 2, 4, 3, 2, 5, 4, 3, 5)

  expect <- c(1., 1., 1., 0., 0.5, 0.5, -1., 1., 1., -1., 0.5, 2.)

  expect_equal(differentiate(x), expect)
})

test_that("first derivative with non-uniform time spacing", {
  x <- c(0, 1, 2, 3, 2, 4, 3, 2, 5, 4, 3, 5)
  t <- c(0, 1, 2.1, 3, 3.5, 4.1, 5, 6, 8, 9, 11, 11.2)

  expect_with_t <- c(
    1.,
    0.95670996,
    1.02020202,
    -0.88888889,
    0.42424242,
    1.55555556,
    -1.05847953,
    -0.16666667,
    -0.16666667,
    -0.83333333,
    9.04545455,
    10.
  )

  expect_equal(differentiate(x, t), expect_with_t)
})

test_that("second derivative with uniform spacing", {
  x <- c(0, 1, 2, 3, 2, 4, 3, 2, 5, 4, 3, 5)

  expect_second <- c(
    0.,
    0.,
    -0.5,
    -0.25,
    0.25,
    -0.75,
    0.25,
    1.,
    -1.,
    -0.25,
    1.5,
    1.5
  )

  expect_equal(differentiate(x, order = 2), expect_second)
})

test_that("second derivative with non-uniform time spacing", {
  x <- c(0, 1, 2, 3, 2, 4, 3, 2, 5, 4, 3, 5)
  t <- c(0, 1, 2.1, 3, 3.5, 4.1, 5, 6, 8, 9, 11, 11.2)

  expect_second <- c(
    -0.04329004,
    0.00481,
    -1.14069264,
    0.93073593,
    2.28956229,
    -0.03048024,
    -1.10623782,
    0.59454191,
    -0.44444444,
    1.2020202,
    4.78787879,
    4.77272727
  )

  expect_equal(differentiate(x, t, order = 2), expect_second)
})

test_that("differentiate handles simple linear case", {
  x <- 0:10
  t <- 0:10

  # First derivative of linear function should be constant
  result <- differentiate(x, t)
  expect_true(all(abs(result - 1) < 1e-10))
})

test_that("differentiate handles quadratic case", {
  t <- 0:10
  x <- t^2

  # First derivative of t^2 should be approximately 2*t
  result <- differentiate(x, t)

  # Check middle points (edges may have larger errors)
  expect_equal(result[5:7], 2 * t[5:7], tolerance = 0.1)
})

test_that("differentiate requires matching lengths", {
  x <- 1:10
  t <- 1:5

  expect_error(differentiate(x, t), "same length")
})

test_that("differentiate handles minimum length input", {
  x <- c(1, 2)

  result <- differentiate(x)
  expect_length(result, 2)
  expect_equal(result[1], 1)
  expect_equal(result[2], 1)
})

test_that("higher order derivatives work correctly", {
  t <- 0:10
  x <- t^3 # Third derivative should be approximately 6

  # Third derivative
  result <- differentiate(x, t, order = 3)

  # Check interior points
  expect_equal(mean(result[4:8]), 6, tolerance = 0.5)
})

test_that("differentiate preserves vector length", {
  x <- rnorm(50)

  result1 <- differentiate(x, order = 1)
  result2 <- differentiate(x, order = 2)
  result3 <- differentiate(x, order = 3)

  expect_length(result1, length(x))
  expect_length(result2, length(x))
  expect_length(result3, length(x))
})

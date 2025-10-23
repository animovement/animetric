test_that("calculate_speed returns correct values for simple cases", {
  # Test straight line movement
  x <- c(0, 1, 2, 3)
  y <- c(0, 0, 0, 0)
  time <- c(0, 1, 2, 3)

  result <- calculate_speed(x, y, time)

  expect_equal(length(result), 4)
  expect_true(is.na(result[1]))
  expect_equal(result[2:4], c(1, 1, 1))
})

test_that("calculate_speed handles diagonal movement correctly", {
  # Movement at 45 degrees: speed should be sqrt(2)
  x <- c(0, 1, 2)
  y <- c(0, 1, 2)
  time <- c(0, 1, 2)

  result <- calculate_speed(x, y, time)

  expect_equal(result[2:3], rep(sqrt(2), 2))
})

test_that("calculate_speed handles varying time intervals", {
  x <- c(0, 2, 4)
  y <- c(0, 0, 0)
  time <- c(0, 1, 3)  # Second interval is twice as long

  result <- calculate_speed(x, y, time)

  expect_equal(result[2], 2)  # 2 units / 1 time = 2
  expect_equal(result[3], 1)  # 2 units / 2 time = 1
})

test_that("calculate_speed handles zero movement", {
  x <- c(0, 0, 0)
  y <- c(0, 0, 0)
  time <- c(0, 1, 2)

  result <- calculate_speed(x, y, time)

  expect_equal(result[2:3], c(0, 0))
})

test_that("calculate_speed handles negative coordinates", {
  x <- c(-1, 0, 1)
  y <- c(-1, 0, 1)
  time <- c(0, 1, 2)

  result <- calculate_speed(x, y, time)

  expect_equal(result[2:3], rep(sqrt(2), 2))
})

test_that("calculate_speed handles single point", {
  x <- 5
  y <- 10
  time <- 0

  result <- calculate_speed(x, y, time)

  expect_equal(length(result), 1)
  expect_true(is.na(result[1]))
})

test_that("calculate_speed handles two points", {
  x <- c(0, 3)
  y <- c(0, 4)
  time <- c(0, 1)

  result <- calculate_speed(x, y, time)

  expect_equal(length(result), 2)
  expect_true(is.na(result[1]))
  expect_equal(result[2], 5)  # 3-4-5 triangle
})

test_that("calculate_speed returns numeric vector", {
  x <- c(0, 1, 2)
  y <- c(0, 1, 2)
  time <- c(0, 1, 2)

  result <- calculate_speed(x, y, time)

  expect_type(result, "double")
  expect_true(is.numeric(result))
})

test_that("calculate_speed handles large values", {
  x <- c(0, 1000, 2000)
  y <- c(0, 1000, 2000)
  time <- c(0, 1, 2)

  result <- calculate_speed(x, y, time)

  expect_equal(result[2:3], rep(1000 * sqrt(2), 2))
})

test_that("calculate_speed handles fractional time intervals", {
  x <- c(0, 1, 2)
  y <- c(0, 0, 0)
  time <- c(0, 0.5, 1)

  result <- calculate_speed(x, y, time)

  expect_equal(result[2:3], c(2, 2))
})

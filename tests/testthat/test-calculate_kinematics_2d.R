# Testing:
# - Translational kinematics (velocity, acceleration, speed, path_length)
# - Rotational kinematics (heading, angular velocity, angular speed)
# - Edge cases (stationary, constant velocity, circular motion)
# - Column presence and structure
# - Consistency with differentiate() function

test_that("calculate_kinematics_2d adds all expected columns", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 0, 0, 0, 0, 0)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  expected_cols <- c(
    "v_x",
    "v_y",
    "a_x",
    "a_y",
    "speed",
    "acceleration",
    "path_length",
    "heading",
    "heading_unwrapped",
    "angular_velocity",
    "angular_speed",
    "angular_acceleration",
    "angular_path_length"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("velocity components match differentiate()", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = sin(seq(0, 2, by = 0.1))
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Calculate expected velocities using differentiate
  expected_v_x <- differentiate(data$x, data$time, order = 1)
  expected_v_y <- differentiate(data$y, data$time, order = 1)

  expect_equal(result$v_x, expected_v_x)
  expect_equal(result$v_y, expected_v_y)
})

test_that("acceleration components match differentiate()", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = sin(seq(0, 2, by = 0.1))
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Calculate expected accelerations using differentiate
  expected_a_x <- differentiate(data$x, data$time, order = 2)
  expected_a_y <- differentiate(data$y, data$time, order = 2)

  expect_equal(result$a_x, expected_a_x)
  expect_equal(result$a_y, expected_a_y)
})

test_that("speed is calculated correctly from velocity components", {
  data <- data.frame(
    time = 0:10,
    x = 0:10 * 3, # v_x = 3
    y = 0:10 * 4 # v_y = 4
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Speed should be sqrt(v_x^2 + v_y^2) = sqrt(9 + 16) = 5
  expected_speed <- sqrt(result$v_x^2 + result$v_y^2)

  expect_equal(result$speed, expected_speed)
})

test_that("acceleration matches differentiate of speed", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = seq(0, 2, by = 0.1)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Acceleration should match differentiate(speed)
  expected_acceleration <- differentiate(result$speed, data$time, order = 1)

  expect_equal(result$acceleration, expected_acceleration)
})

test_that("path_length accumulates distance correctly", {
  # Create a simple rectangular path
  data <- data.frame(
    time = 0:4,
    x = c(0, 3, 3, 0, 0),
    y = c(0, 0, 4, 4, 0)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Manual calculation: 3 + 4 + 3 + 4 = 14
  dx <- diff(data$x)
  dy <- diff(data$y)
  expected_path <- cumsum(c(0, sqrt(dx^2 + dy^2)))

  expect_equal(result$path_length, expected_path)
})

test_that("heading is calculated correctly from velocity", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 1, 2, 3, 4, 5)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # For 45-degree motion, heading should be pi/4
  expected_heading <- atan2(result$v_y, result$v_x)
  expected_heading <- ifelse(expected_heading == pi, 0, expected_heading)

  expect_equal(result$heading, expected_heading)
})

test_that("angular_velocity matches differentiate of unwrapped heading", {
  # Create circular motion
  t <- seq(0, 2 * pi, length.out = 50)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Angular velocity should match differentiate(heading_unwrapped)
  expected_ang_vel <- differentiate(
    result$heading_unwrapped,
    data$time,
    order = 1
  )

  expect_equal(result$angular_velocity, expected_ang_vel)
})

test_that("angular_speed is absolute value of angular_velocity", {
  t <- seq(0, 4 * pi, length.out = 100)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  expect_equal(result$angular_speed, abs(result$angular_velocity))
})

test_that("angular_acceleration matches differentiate of unwrapped heading", {
  t <- seq(0, 2 * pi, length.out = 50)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Angular acceleration should match second derivative of heading
  expected_ang_acc <- differentiate(
    result$heading_unwrapped,
    data$time,
    order = 2
  )

  expect_equal(result$angular_acceleration, expected_ang_acc)
})

test_that("stationary object has zero kinematics", {
  data <- data.frame(
    time = 0:5,
    x = rep(5, 6),
    y = rep(3, 6)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # All velocities and speed should be zero (except potentially first point)
  expect_true(all(result$speed[2:6] == 0))
  expect_equal(result$path_length[6], 0)
})

test_that("constant velocity has zero acceleration", {
  data <- data.frame(
    time = 0:10,
    x = (0:10) * 2,
    y = (0:10) * 3
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_2d(data)

  # Acceleration should be approximately zero for constant velocity
  # (excluding edge effects from differentiate)
  expect_true(all(abs(result$acceleration[3:9]) < 1e-10))
})

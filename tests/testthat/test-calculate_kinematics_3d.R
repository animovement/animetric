# Testing:
# - Translational kinematics in 3D (velocity, acceleration, speed, path_length)
# - 3D velocity components (v_x, v_y, v_z)
# - 3D acceleration components (a_x, a_y, a_z)
# - Edge cases (stationary, constant velocity)
# - Consistency with differentiate() function

test_that("calculate_kinematics_3d adds all expected columns", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 0, 0, 0, 0, 0),
    z = c(0, 0, 0, 0, 0, 0)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  expected_cols <- c(
    "v_x",
    "v_y",
    "v_z",
    "a_x",
    "a_y",
    "a_z",
    "speed",
    "acceleration",
    "path_length"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("3D velocity components match differentiate()", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = sin(seq(0, 2, by = 0.1)),
    z = seq(0, 2, by = 0.1) * 2
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Calculate expected velocities using differentiate
  expected_v_x <- differentiate(data$x, data$time, order = 1)
  expected_v_y <- differentiate(data$y, data$time, order = 1)
  expected_v_z <- differentiate(data$z, data$time, order = 1)

  expect_equal(result$v_x, expected_v_x)
  expect_equal(result$v_y, expected_v_y)
  expect_equal(result$v_z, expected_v_z)
})

test_that("3D acceleration components match differentiate()", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = sin(seq(0, 2, by = 0.1)),
    z = seq(0, 2, by = 0.1) * 3
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Calculate expected accelerations using differentiate
  expected_a_x <- differentiate(data$x, data$time, order = 2)
  expected_a_y <- differentiate(data$y, data$time, order = 2)
  expected_a_z <- differentiate(data$z, data$time, order = 2)

  expect_equal(result$a_x, expected_a_x)
  expect_equal(result$a_y, expected_a_y)
  expect_equal(result$a_z, expected_a_z)
})

test_that("3D speed is calculated correctly from velocity components", {
  data <- data.frame(
    time = 0:10,
    x = (0:10) * 2, # v_x = 2
    y = (0:10) * 3, # v_y = 3
    z = (0:10) * 6 # v_z = 6
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Speed should be sqrt(v_x^2 + v_y^2 + v_z^2)
  expected_speed <- sqrt(result$v_x^2 + result$v_y^2 + result$v_z^2)

  expect_equal(result$speed, expected_speed)
})

test_that("3D acceleration matches differentiate of speed", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = seq(0, 2, by = 0.1),
    z = seq(0, 2, by = 0.1)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Acceleration should match differentiate(speed)
  expected_acceleration <- differentiate(result$speed, data$time, order = 1)

  expect_equal(result$acceleration, expected_acceleration)
})

test_that("3D path_length accumulates distance correctly", {
  # Create a simple 3D path
  data <- data.frame(
    time = 0:3,
    x = c(0, 3, 3, 3),
    y = c(0, 0, 4, 4),
    z = c(0, 0, 0, 5)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Manual calculation
  dx <- diff(data$x)
  dy <- diff(data$y)
  dz <- diff(data$z)
  expected_path <- cumsum(c(0, sqrt(dx^2 + dy^2 + dz^2)))

  expect_equal(result$path_length, expected_path)
})

test_that("stationary 3D object has zero kinematics", {
  data <- data.frame(
    time = 0:5,
    x = rep(5, 6),
    y = rep(3, 6),
    z = rep(2, 6)
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # All velocities should be zero
  expect_true(all(result$speed[2:6] == 0))
  expect_equal(result$path_length[6], 0)
})

test_that("constant 3D velocity has zero acceleration", {
  data <- data.frame(
    time = 0:10,
    x = (0:10) * 2,
    y = (0:10) * 3,
    z = (0:10) * 4
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # Acceleration should be approximately zero (excluding edge effects)
  expect_true(all(abs(result$acceleration[3:9]) < 1e-10))
})

test_that("helical motion produces expected velocity structure", {
  # Helical path: x = cos(t), y = sin(t), z = t
  t <- seq(0, 2 * pi, length.out = 100)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t),
    z = t
  ) |>
    aniframe::as_aniframe()

  result <- calculate_kinematics_3d(data)

  # v_z should be approximately constant
  v_z_sd <- sd(result$v_z, na.rm = TRUE)
  expect_true(v_z_sd < 0.1)

  # Speed should be approximately constant
  speed_sd <- sd(result$speed, na.rm = TRUE)
  expect_true(speed_sd < 0.1)
})

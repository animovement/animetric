# Testing:
# - Translational kinematics (velocity, acceleration, speed, path_length)
# - Path direction (course, turning rate, turning speed)
# - Edge cases (stationary, constant velocity, circular motion)
# - Column presence and structure
# - Consistency with differentiate() function

test_that("calculate_kinematics() on 2D data adds all expected columns", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 0, 0, 0, 0, 0)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expected_cols <- c(
    "v_x",
    "v_y",
    "a_x",
    "a_y",
    "speed",
    "acceleration",
    "path_length",
    "course",
    "course_unwrapped",
    "turning_rate",
    "turning_speed",
    "turning_acceleration",
    "cumulative_turning"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("velocity components match differentiate()", {
  data <- data.frame(
    time = seq(0, 2, by = 0.1),
    x = seq(0, 2, by = 0.1)^2,
    y = sin(seq(0, 2, by = 0.1))
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

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
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

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
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

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
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

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
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  # Manual calculation: 3 + 4 + 3 + 4 = 14
  dx <- diff(data$x)
  dy <- diff(data$y)
  expected_path <- cumsum(c(0, sqrt(dx^2 + dy^2)))

  expect_equal(result$path_length, expected_path)
})

test_that("course is calculated correctly from velocity", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 1, 2, 3, 4, 5)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  # For 45-degree motion, course should be pi/4
  expected_course <- atan2(result$v_y, result$v_x)

  expect_equal(result$course, expected_course)
})

test_that("course along -x is pi, not 0", {
  data <- data.frame(
    time = 0:5,
    x = 5:0,
    y = rep(0, 6)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$course, rep(pi, 6))
  expect_equal(result$turning_rate, rep(0, 6))
  expect_equal(result$cumulative_turning, rep(0, 6))
})

test_that("course is NA where the animal is stationary", {
  # Moves along -x, pauses at x = 2, then moves on along -x
  data <- data.frame(
    time = 0:8,
    x = c(5, 4, 3, 2, 2, 2, 1, 0, -1),
    y = rep(0, 9)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  stationary <- result$speed == 0
  expect_true(any(stationary))
  expect_true(all(is.na(result$course[stationary])))
  expect_equal(result$course[!stationary], rep(pi, sum(!stationary)))

  # The pause must not register as turning
  expect_equal(max(result$cumulative_turning), 0)
})

test_that("turning_rate is the signed rate of change of the course", {
  # A unit circle at unit angular speed turns at 1 rad per unit time:
  # positive anticlockwise, negative clockwise
  t <- seq(0, 2 * pi, length.out = 200)
  interior <- 3:198
  anticlockwise <- data.frame(time = t, x = cos(t), y = sin(t)) |>
    anicore::as_anipoint()
  clockwise <- data.frame(time = t, x = cos(t), y = -sin(t)) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(anticlockwise)
  expect_equal(result$turning_rate[interior], rep(1, 196), tolerance = 1e-3)
  expect_equal(
    calculate_kinematics(clockwise)$turning_rate[interior],
    rep(-1, 196),
    tolerance = 1e-3
  )

  # It is the derivative of the unwrapped course
  expect_equal(
    result$turning_rate[interior],
    differentiate(result$course_unwrapped, t)[interior],
    tolerance = 1e-3
  )
})

test_that("turning_rate scales with curvature and speed", {
  # Radius 2 at speed 4: curvature 1/2, so 2 rad per unit time
  t <- seq(0, pi, length.out = 200)
  data <- data.frame(time = t, x = 2 * cos(2 * t), y = 2 * sin(2 * t)) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$turning_rate[3:198], rep(2, 196), tolerance = 1e-3)
  expect_equal(result$speed[3:198], rep(4, 196), tolerance = 1e-3)
})

test_that("turning_speed is absolute value of turning_rate", {
  t <- seq(0, 4 * pi, length.out = 100)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$turning_speed, abs(result$turning_rate))
})

test_that("turning_acceleration is the derivative of the turning rate", {
  t <- seq(0, 2 * pi, length.out = 50)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = sin(t)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(
    result$turning_acceleration,
    differentiate(result$turning_rate, data$time)
  )
})

test_that("stationary object has zero kinematics", {
  data <- data.frame(
    time = 0:5,
    x = rep(5, 6),
    y = rep(3, 6)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

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
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  # Acceleration should be approximately zero for constant velocity
  # (excluding edge effects from differentiate)
  expect_true(all(abs(result$acceleration[3:9]) < 1e-10))
})

test_that("cumulative_turning starts at 0 when the first course is negative", {
  # Straight line at a course of -1 rad: it never turns
  t <- 0:5
  data <- data.frame(
    time = t,
    x = t * cos(-1),
    y = t * sin(-1)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$course, rep(-1, 6))
  expect_equal(result$cumulative_turning, rep(0, 6))
})

test_that("cumulative_turning accumulates absolute turning", {
  t <- seq(0, 2 * pi, length.out = 50)
  data <- data.frame(
    time = t,
    x = cos(t),
    y = -sin(t)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$cumulative_turning[1], 0)
  expect_equal(
    result$cumulative_turning,
    cumsum(c(0, abs(diff(result$course_unwrapped))))
  )
})

test_that("cumulative_turning counts turning across a pause", {
  # Moves along +x, stops, then moves back along -x
  data <- data.frame(
    time = 0:8,
    x = c(0, 1, 2, 3, 3, 3, 2, 1, 0),
    y = rep(0, 9)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_true(any(is.na(result$course)))
  expect_false(anyNA(result$cumulative_turning))
  expect_equal(result$cumulative_turning[1], 0)
  expect_equal(dplyr::last(result$cumulative_turning), pi)
})

test_that("cumulative_turning is 0 for a stationary track", {
  data <- data.frame(
    time = 0:5,
    x = rep(5, 6),
    y = rep(3, 6)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_equal(result$cumulative_turning, rep(0, 6))
})

# Angular units and sign convention (#80) ------------------------------------

test_that("angular measures are returned in the frame's unit_angle", {
  t <- seq(0, 2 * pi, length.out = 40)
  d <- data.frame(time = t, x = cos(t) + t / 4, y = sin(t))
  rad <- anicore::as_anipoint(d)
  deg <- anicore::set_metadata(rad, unit_angle = "deg")

  in_rad <- calculate_kinematics(rad)
  in_deg <- calculate_kinematics(deg)

  angular_cols <- c(
    "course",
    "course_unwrapped",
    "turning_rate",
    "turning_speed",
    "turning_acceleration",
    "cumulative_turning"
  )
  for (col in angular_cols) {
    expect_equal(in_deg[[col]], in_rad[[col]] * 180 / pi, label = col)
  }
  # Translational measures are untouched
  expect_equal(in_deg$speed, in_rad$speed)
  expect_equal(as.character(anicore::get_metadata(in_deg, "unit_angle")), "deg")
})

test_that("course summaries are in the frame's unit_angle", {
  t <- seq(0, 2 * pi, length.out = 40)
  d <- data.frame(time = t, x = cos(t) + t / 4, y = sin(t))
  rad <- calculate_kinematics(anicore::as_anipoint(d))
  deg <- calculate_kinematics(
    anicore::set_metadata(anicore::as_anipoint(d), unit_angle = "deg")
  )

  for (measures in c("median_mad", "mean_sd")) {
    s_rad <- summarise_aniframe(rad, measures = measures)
    s_deg <- summarise_aniframe(deg, measures = measures)
    angular <- grep("course|turning", names(s_rad), value = TRUE)
    expect_length(angular, 8)
    for (col in angular) {
      expect_equal(s_deg[[col]], s_rad[[col]] * 180 / pi, label = col)
    }
  }

  expect_equal(
    summarise_path(deg)$total_turning,
    summarise_path(rad)$total_turning * 180 / pi
  )
})

test_that("signed angles follow the frame's own axes", {
  # Moving +x then turning toward +y. In a y-down frame that is clockwise on
  # screen, and the angles say so in the frame's own terms: they match the
  # coordinates, not a fixed physical sense.
  d <- data.frame(
    time = 0:6,
    x = c(0, 1, 2, 3, 3, 3, 3),
    y = c(0, 0, 0, 0, 1, 2, 3)
  )
  up <- anicore::set_axis_directions(
    anicore::as_anipoint(d),
    c(x = "right", y = "up")
  )
  down <- anicore::set_axis_directions(
    anicore::as_anipoint(d),
    c(x = "right", y = "down")
  )

  expect_equal(anicore::get_angle_direction(up), "counter_clockwise")
  expect_equal(anicore::get_angle_direction(down), "clockwise")
  expect_equal(
    calculate_kinematics(down)$course,
    calculate_kinematics(up)$course
  )
  expect_equal(dplyr::last(calculate_kinematics(up)$course), pi / 2)
})

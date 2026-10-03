# Testing:
# - Coordinate system detection and preservation
# - Conversion to/from Cartesian for non-Cartesian inputs
# - Routing to correct 2D/3D calculation function
# - Error handling for invalid inputs

test_that("calculate_kinematics preserves Cartesian 2D coordinate system", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 0, 0, 0, 0, 0)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_true(anicore::is_cartesian_2d(result))
  expect_true("speed" %in% names(result))
  expect_true("heading" %in% names(result))
})

test_that("calculate_kinematics preserves Cartesian 3D coordinate system", {
  data <- data.frame(
    time = 0:5,
    x = c(0, 1, 2, 3, 4, 5),
    y = c(0, 0, 0, 0, 0, 0),
    z = c(0, 0, 0, 0, 0, 0)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_true(anicore::is_cartesian_3d(result))
  expect_true("speed" %in% names(result))
  expect_true("v_z" %in% names(result))
})

test_that("calculate_kinematics converts polar to Cartesian and back", {
  # Create polar data
  data_cartesian <- data.frame(
    time = 0:5,
    x = c(1, 2, 3, 4, 5, 6),
    y = c(0, 0, 0, 0, 0, 0)
  ) |>
    anicore::as_anipoint()

  data_polar <- anispace::map_to_polar(data_cartesian)
  result <- calculate_kinematics(data_polar)

  expect_true(anicore::is_polar(result))
  expect_true("rho" %in% names(result))
  expect_true("phi" %in% names(result))
  expect_false("x" %in% names(result))
})

# test_that("calculate_kinematics converts cylindrical to Cartesian and back", {
#   data_cartesian <- data.frame(
#     time = 0:5,
#     x = c(1, 2, 3, 4, 5, 6),
#     y = c(0, 0, 0, 0, 0, 0),
#     z = c(0, 1, 2, 3, 4, 5)
#   ) |>
#     anicore::as_anipoint()

#   data_cylindrical <- anispace::map_to_cylindrical(data_cartesian)
#   result <- calculate_kinematics(data_cylindrical)

#   expect_true(anicore::is_cylindrical(result))
#   expect_true("rho" %in% names(result))
#   expect_true("phi" %in% names(result))
#   expect_true("z" %in% names(result))
# })

test_that("calculate_kinematics converts spherical to Cartesian and back", {
  data_cartesian <- data.frame(
    time = 0:5,
    x = c(1, 2, 3, 4, 5, 6),
    y = c(0, 0, 0, 0, 0, 0),
    z = c(0, 1, 2, 3, 4, 5)
  ) |>
    anicore::as_anipoint()

  data_spherical <- anispace::map_to_spherical(data_cartesian)
  result <- calculate_kinematics(data_spherical)

  expect_true(anicore::is_spherical(result))
  expect_true("rho" %in% names(result))
  expect_true("phi" %in% names(result))
  expect_true("theta" %in% names(result))
})

test_that("calculate_kinematics requires aniframe input", {
  data <- data.frame(time = 0:5, x = 0:5, y = 0:5)
  expect_error(calculate_kinematics(data))
})

# Frames whose columns are not called x, y, time (#81) ----------------------

test_that("calculate_kinematics() reads renamed axis columns from the frame", {
  d <- data.frame(time = 0:9, x = cumsum(1:10), y = sin(0:9))
  standard <- anicore::as_anipoint(d)
  renamed <- anicore::as_anipoint(
    dplyr::rename(d, u = "x", v = "y"),
    variables_where = c(x = "u", y = "v")
  )

  expected <- calculate_kinematics(standard)
  result <- calculate_kinematics(renamed)

  # Components are named by axis role, whatever the columns are called
  kinematic_cols <- c(
    "speed",
    "acceleration",
    "path_length",
    "v_x",
    "v_y",
    "a_x",
    "a_y",
    "heading",
    "angular_velocity",
    "angular_path_length"
  )
  for (col in kinematic_cols) {
    expect_equal(result[[col]], expected[[col]], label = col)
  }
})

test_that("calculate_kinematics() reads a renamed index from the frame", {
  d <- data.frame(time = c(0, 0.5, 1.5, 2, 3), x = c(0, 1, 3, 4, 7), y = 0)
  standard <- anicore::as_anipoint(d)
  renamed <- anicore::as_anipoint(
    dplyr::rename(d, frame = "time"),
    index = "frame"
  )

  expect_equal(
    calculate_kinematics(renamed)$speed,
    calculate_kinematics(standard)$speed
  )
})

test_that("calculate_kinematics() computes translation for 1D data", {
  data <- anicore::as_anipoint(data.frame(time = 0:5, x = c(0, 1, 3, 6, 6, 4)))
  expect_true(anicore::is_cartesian_1d(data))

  result <- calculate_kinematics(data)

  expect_true(is_aniframe_kin(result))
  expect_equal(result$v_x, differentiate(data$x, data$time))
  expect_equal(result$speed, abs(result$v_x))
  expect_equal(result$path_length, c(0, 1, 3, 6, 6, 8))
  expect_false("heading" %in% names(result))
})

test_that("summaries work on 1D data", {
  data <- anicore::as_anipoint(data.frame(time = 0:9, x = c(0:5, 4:1)))
  kin <- calculate_kinematics(data)

  kin_summary <- summarise_kinematics(kin)
  expect_true(all(
    c("median_speed", "mad_acceleration") %in% names(kin_summary)
  ))
  expect_false("median_heading" %in% names(kin_summary))

  tort_summary <- summarise_tortuosity(kin)
  expect_equal(tort_summary$total_path_length, 9)
  expect_equal(tort_summary$net_displacement, 1)
})

test_that("calculate_kinematics() returns cylindrical input in cylindrical coordinates", {
  cartesian <- data.frame(
    time = 0:9,
    x = cumsum(1:10),
    y = sin(0:9),
    z = 0:9 / 2
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(anispace::map_to_cylindrical(cartesian))

  expect_true(anicore::is_cylindrical(result))
  expect_equal(result$speed, calculate_kinematics(cartesian)$speed)
})

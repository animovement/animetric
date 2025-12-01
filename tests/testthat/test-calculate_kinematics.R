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
    aniframe::as_aniframe()

  result <- calculate_kinematics(data)

  expect_true(aniframe::is_cartesian_2d(result))
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
    aniframe::as_aniframe()

  result <- calculate_kinematics(data)

  expect_true(aniframe::is_cartesian_3d(result))
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
    aniframe::as_aniframe()

  data_polar <- anispace::map_to_polar(data_cartesian)
  result <- calculate_kinematics(data_polar)

  expect_true(aniframe::is_polar(result))
  expect_true("rho" %in% names(result))
  expect_true("phi" %in% names(result))
  expect_false("x" %in% names(result))
})

test_that("calculate_kinematics converts cylindrical to Cartesian and back", {
  data_cartesian <- data.frame(
    time = 0:5,
    x = c(1, 2, 3, 4, 5, 6),
    y = c(0, 0, 0, 0, 0, 0),
    z = c(0, 1, 2, 3, 4, 5)
  ) |>
    aniframe::as_aniframe()

  data_cylindrical <- anispace::map_to_cylindrical(data_cartesian)
  result <- calculate_kinematics(data_cylindrical)

  expect_true(aniframe::is_cylindrical(result))
  expect_true("rho" %in% names(result))
  expect_true("phi" %in% names(result))
  expect_true("z" %in% names(result))
})

test_that("calculate_kinematics converts spherical to Cartesian and back", {
  data_cartesian <- data.frame(
    time = 0:5,
    x = c(1, 2, 3, 4, 5, 6),
    y = c(0, 0, 0, 0, 0, 0),
    z = c(0, 1, 2, 3, 4, 5)
  ) |>
    aniframe::as_aniframe()

  data_spherical <- anispace::map_to_spherical(data_cartesian)
  result <- calculate_kinematics(data_spherical)

  expect_true(aniframe::is_spherical(result))
  expect_true("rho" %in% names(result))
  expect_true("phi" %in% names(result))
  expect_true("theta" %in% names(result))
})

test_that("calculate_kinematics requires aniframe input", {
  data <- data.frame(time = 0:5, x = 0:5, y = 0:5)
  expect_error(calculate_kinematics(data))
})

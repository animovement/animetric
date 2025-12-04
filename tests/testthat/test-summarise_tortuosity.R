# Tests for summarise_tortuosity
#
# - summarise_tortuosity returns correct columns for 2D data
# - summarise_tortuosity returns correct columns for 3D data
# - summarise_tortuosity includes angular_path_length only for 2D
# - summarise_tortuosity preserves grouping structure
# - summarise_tortuosity validates input with .check = TRUE
# - summarise_tortuosity_2d computes total_path_length correctly
# - summarise_tortuosity_2d computes net_displacement correctly
# - summarise_tortuosity_2d computes straightness correctly
# - summarise_tortuosity_3d computes total_path_length correctly
# - summarise_tortuosity_3d computes net_displacement correctly
# - straightness is 1 for straight path
# - straightness is less than 1 for non-straight path
# - summarise_tortuosity calculates kinematics when given plain aniframe
# - summarise_tortuosity produces same result for plain vs pre-calculated input
# - summarise_tortuosity works with plain 3D aniframe
# - summarise_tortuosity works with grouped plain aniframe

# Helper to create mock 2D kinematics aniframe
mock_kin_2d <- function(n = 10, grouped = FALSE, calculate_kinematics = TRUE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  data <- aniframe::as_aniframe(data)
  if (calculate_kinematics == TRUE) {
    data <- calculate_kinematics(data)
  }
  data
}

# Helper to create mock 3D kinematics aniframe
mock_kin_3d <- function(n = 10, grouped = FALSE, calculate_kinematics = TRUE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n)),
    z = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  data <- aniframe::as_aniframe(data)
  if (calculate_kinematics == TRUE) {
    data <- calculate_kinematics(data)
  }
  data
}

# Helper to create a straight-line path
mock_straight_path_2d <- function(n = 10, calculate_kinematics = TRUE) {
  x <- seq(0, 10, length.out = n)
  y <- seq(0, 10, length.out = n)

  data <- data.frame(
    time = seq_len(n),
    x = x,
    y = y
  )

  data <- aniframe::as_aniframe(data)
  if (calculate_kinematics == TRUE) {
    data <- calculate_kinematics(data)
  }
  data
}


# summarise_tortuosity: 2D output columns --------------------------------

test_that("summarise_tortuosity returns correct columns for 2D data", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity(data)

  expected_cols <- c(
    "total_path_length",
    "total_angular_path_length",
    "net_displacement",
    "straightness",
    "sinuosity",
    "emax"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_tortuosity: 3D output columns --------------------------------

test_that("summarise_tortuosity returns correct columns for 3D data", {
  data <- mock_kin_3d()
  result <- summarise_tortuosity(data)

  expected_cols <- c(
    "total_path_length",
    "net_displacement",
    "straightness",
    "sinuosity",
    "emax"
  )
  excluded_cols <- "total_angular_path_length"

  expect_true(all(expected_cols %in% names(result)))
  expect_false(any(excluded_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})


# summarise_tortuosity: grouping -----------------------------------------

test_that("summarise_tortuosity preserves grouping structure for 2D", {
  data <- mock_kin_2d(grouped = TRUE)
  result <- summarise_tortuosity(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})

test_that("summarise_tortuosity preserves grouping structure for 3D", {
  data <- mock_kin_3d(grouped = TRUE)
  result <- summarise_tortuosity(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})


# summarise_tortuosity: computation correctness --------------------------

test_that("summarise_tortuosity_2d computes total_path_length correctly", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity_2d(data)

  expected <- dplyr::last(data$path_length) - dplyr::first(data$path_length)
  expect_equal(result$total_path_length, expected)
})

test_that("summarise_tortuosity_2d computes net_displacement correctly", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity_2d(data)

  expected <- sqrt(
    (dplyr::last(data$x) - dplyr::first(data$x))^2 +
      (dplyr::last(data$y) - dplyr::first(data$y))^2
  )
  expect_equal(result$net_displacement, expected)
})

test_that("summarise_tortuosity_3d computes total_path_length correctly", {
  data <- mock_kin_3d()
  result <- summarise_tortuosity_3d(data)

  expected <- dplyr::last(data$path_length) - dplyr::first(data$path_length)
  expect_equal(result$total_path_length, expected)
})

test_that("summarise_tortuosity_3d computes net_displacement correctly", {
  data <- mock_kin_3d()
  result <- summarise_tortuosity_3d(data)

  expected <- sqrt(
    (dplyr::last(data$x) - dplyr::first(data$x))^2 +
      (dplyr::last(data$y) - dplyr::first(data$y))^2 +
      (dplyr::last(data$z) - dplyr::first(data$z))^2
  )
  expect_equal(result$net_displacement, expected)
})


# summarise_tortuosity: straightness -------------------------------------

test_that("straightness is 1 for a perfectly straight path", {
  data <- mock_straight_path_2d()
  result <- summarise_tortuosity(data)

  expect_equal(result$straightness, 1, tolerance = 1e-10)
})

test_that("straightness is between 0 and 1 for non-straight paths", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity(data)

  expect_true(result$straightness >= 0)
  expect_true(result$straightness <= 1)
})


# summarise_tortuosity: sinuosity and emax -------------------------------

test_that("sinuosity and emax are numeric and finite", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity(data)

  expect_true(is.numeric(result$sinuosity))
  expect_true(is.numeric(result$emax))
  expect_true(is.finite(result$sinuosity))
  expect_true(is.finite(result$emax))
})

test_that("sinuosity and emax work for 3D data", {
  data <- mock_kin_3d()
  result <- summarise_tortuosity(data)

  expect_true(is.numeric(result$sinuosity))
  expect_true(is.numeric(result$emax))
})


# summarise_tortuosity: internal columns removed -------------------------

test_that("summarise_tortuosity_2d removes internal columns", {
  data <- mock_kin_2d()
  result <- summarise_tortuosity_2d(data)

  internal_cols <- c(
    ".first_x",
    ".first_y",
    ".last_x",
    ".last_y",
    ".mean_cos_turning",
    ".mean_step_length",
    ".n_steps"
  )

  expect_false(any(internal_cols %in% names(result)))
})

test_that("summarise_tortuosity_3d removes internal columns", {
  data <- mock_kin_3d()
  result <- summarise_tortuosity_3d(data)

  internal_cols <- c(
    ".first_x",
    ".first_y",
    ".first_z",
    ".last_x",
    ".last_y",
    ".last_z",
    ".mean_cos_turning",
    ".mean_step_length",
    ".n_steps"
  )

  expect_false(any(internal_cols %in% names(result)))
})

# summarise_tortuosity: auto-calculation ---------------------------------

test_that("summarise_tortuosity calculates kinematics when given plain aniframe", {
  data <- mock_kin_2d(calculate_kinematics = FALSE)
  result <- summarise_tortuosity(data)

  expected_cols <- c(
    "total_path_length",
    "total_angular_path_length",
    "net_displacement",
    "straightness",
    "sinuosity",
    "emax"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_tortuosity produces same result for plain vs pre-calculated input", {
  set.seed(123)
  data_plain <- mock_kin_2d(calculate_kinematics = FALSE)

  set.seed(123)
  data_kin <- mock_kin_2d(calculate_kinematics = FALSE) |>
    calculate_kinematics() |>
    calculate_tortuosity()

  result_plain <- summarise_tortuosity(data_plain)
  result_kin <- summarise_tortuosity(data_kin)

  expect_equal(result_plain$total_path_length, result_kin$total_path_length)
  expect_equal(result_plain$net_displacement, result_kin$net_displacement)
  expect_equal(result_plain$straightness, result_kin$straightness)
})

test_that("summarise_tortuosity works with plain 3D aniframe", {
  data <- mock_kin_3d(calculate_kinematics = FALSE)
  result <- summarise_tortuosity(data)

  expected_cols <- c(
    "total_path_length",
    "net_displacement",
    "straightness",
    "sinuosity",
    "emax"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_false("total_angular_path_length" %in% names(result))
  expect_equal(nrow(result), 1L)
})

test_that("summarise_tortuosity works with grouped plain aniframe", {
  data <- mock_kin_2d(grouped = TRUE, calculate_kinematics = FALSE)
  result <- summarise_tortuosity(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
  expect_true("total_path_length" %in% names(result))
})

test_that("summarise_tortuosity works with grouped plain 3D aniframe", {
  data <- mock_kin_3d(grouped = TRUE, calculate_kinematics = FALSE)
  result <- summarise_tortuosity(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})

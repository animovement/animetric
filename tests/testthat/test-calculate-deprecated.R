# calculate_kinematics() and calculate_tortuosity() are deprecated in favour
# of add_kinematics() and add_tortuosity() (#103), and return exactly what
# they did.

zigzag <- function(n = 20) {
  data.frame(
    time = seq_len(n),
    x = seq_len(n),
    y = rep(c(0, 1), length.out = n)
  ) |>
    anicore::as_anipoint()
}

test_that("calculate_kinematics() warns and keeps path_length", {
  data <- zigzag()

  expect_warning(
    old <- calculate_kinematics(data),
    class = "lifecycle_warning_deprecated"
  )
  new <- add_kinematics(data, min_step = 0)

  expect_false("cumulative_distance" %in% names(old))
  expect_equal(
    names(old),
    sub("^cumulative_distance$", "path_length", names(new))
  )
  expect_equal(old$path_length, new$cumulative_distance)
  expect_equal(
    dplyr::rename(old, cumulative_distance = "path_length"),
    new
  )
})

# A spiral, and what calculate_tortuosity() and summarise_tortuosity()
# returned for it before #104
spiral <- function() {
  t <- seq(0, 3 * pi, length.out = 40)
  data.frame(time = seq_along(t), x = t * cos(t), y = t * sin(t)) |>
    anicore::as_anipoint()
}

test_that("calculate_kinematics() still counts every direction", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  # Short steps near the centre of the spiral count too
  expect_equal(
    dplyr::last(calculate_kinematics(spiral())$cumulative_turning),
    10.52653024,
    tolerance = 1e-8
  )
})

test_that("calculate_kinematics() passes vertical on", {
  data <- data.frame(
    time = 1:10,
    x = cos(1:10),
    y = sin(1:10),
    z = 1:10
  ) |>
    anicore::as_anipoint()

  expect_warning(
    old <- calculate_kinematics(data, vertical = "z"),
    class = "lifecycle_warning_deprecated"
  )
  expect_equal(old$course, add_kinematics(data, vertical = "z")$course)
})

test_that("calculate_tortuosity() warns and keeps its old columns", {
  data <- zigzag()

  expect_warning(
    old <- calculate_tortuosity(data, window_width = 5L),
    class = "lifecycle_warning_deprecated"
  )
  new <- add_tortuosity(data, window_width = 5L)

  # The kinematics it computed on the way are added, with the old names
  kin <- suppressWarnings(calculate_kinematics(data))
  expect_named(old, c(names(kin), "straightness", "sinuosity", "emax"))
  expect_equal(old$straightness, new$straightness_5)
})

test_that("calculate_tortuosity() keeps its frame-by-frame sinuosity", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  old <- calculate_tortuosity(spiral(), window_width = 5L)
  rows <- c(3, 10, 20, 30, 38)

  expect_equal(
    old$sinuosity[rows],
    c(0.7445016017, 0.3773780555, 0.2385387069, 0.1895937972, 0.1501548903),
    tolerance = 1e-8
  )
  expect_equal(
    old$emax[rows],
    c(5.140300026, 6.975463765, 7.878009756, 8.075296989, 9.044280346),
    tolerance = 1e-8
  )
})

test_that("summarise_tortuosity() keeps its old values", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  expect_equal(
    as.list(summarise_tortuosity(spiral())[-1]),
    list(
      total_path_length = 46.00331841,
      total_turning = 10.52653024,
      net_displacement = 9.424777961,
      straightness = 0.2048716981,
      sinuosity = 0.2550795578,
      emax = 7.219249701
    ),
    tolerance = 1e-8
  )
})

test_that("calculate_tortuosity() uses kinematics already present", {
  kin <- add_kinematics(zigzag())

  expect_warning(
    old <- calculate_tortuosity(kin, window_width = 5L),
    class = "lifecycle_warning_deprecated"
  )
  expect_named(old, c(names(kin), "straightness", "sinuosity", "emax"))
})

test_that("calculate_tortuosity() checks its input as before", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  polar <- anispace::map_to_polar(zigzag())

  expect_error(calculate_tortuosity(polar), "Cartesian")
  expect_error(
    calculate_tortuosity(zigzag(), window_width = 2L),
    "window_width"
  )
})

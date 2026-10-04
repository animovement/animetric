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
  new <- add_kinematics(data)

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
  expect_equal(old$sinuosity, new$sinuosity_5)
  expect_equal(old$emax, new$e_max_5)
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

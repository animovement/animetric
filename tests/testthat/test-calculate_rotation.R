# Turning measures of the path in 2D and 3D, and the vertical they need.

helix <- function(t = seq(0, 4 * pi, length.out = 400)) {
  data.frame(time = t, x = cos(t), y = sin(t), z = t) |>
    anicore::as_anipoint()
}
interior <- 3:398

test_that("3D gets turning speed and cumulative turning without a vertical", {
  result <- calculate_kinematics(helix())

  expect_true(all(c("turning_speed", "cumulative_turning") %in% names(result)))
  expect_false(any(
    c("course", "course_elevation", "turning_rate") %in% names(result)
  ))

  # A helix of radius 1 and pitch 2*pi: curvature 1/2 at speed sqrt(2)
  expect_equal(
    result$turning_speed[interior],
    rep(sqrt(2) / 2, length(interior)),
    tolerance = 1e-3
  )
})

test_that("a vertical adds course, elevation and the horizontal turning rate", {
  result <- calculate_kinematics(helix(), vertical = "z")

  # Climbing at 45 degrees, turning anticlockwise seen from above at 1 rad/s
  expect_equal(
    result$course_elevation[interior],
    rep(pi / 4, length(interior)),
    tolerance = 1e-3
  )
  expect_equal(
    result$turning_rate[interior],
    rep(1, length(interior)),
    tolerance = 1e-3
  )
  expect_equal(
    result$turning_acceleration,
    differentiate(result$turning_rate, result$time)
  )
  # Course is a quarter turn ahead of the position angle t
  expect_equal(
    result$course[interior],
    anicore::wrap_angle(result$time + pi / 2, "pi")[interior],
    tolerance = 1e-3
  )
  expect_equal(result$course_unwrapped, anicore::unwrap_angle(result$course))
})

test_that("course turns about the vertical by the right-hand rule", {
  t <- seq(0, 2 * pi, length.out = 200)
  # The same circle, laid in the horizontal plane of each choice of vertical
  # so that it runs anticlockwise about it
  around_y <- data.frame(time = t, x = sin(t), y = 0, z = cos(t)) |>
    anicore::as_anipoint()
  around_x <- data.frame(time = t, x = 0, y = cos(t), z = sin(t)) |>
    anicore::as_anipoint()

  for (case in list(list(around_y, "y"), list(around_x, "x"))) {
    result <- calculate_kinematics(case[[1]], vertical = case[[2]])
    expect_equal(result$turning_rate[3:198], rep(1, 196), tolerance = 1e-3)
    expect_equal(result$course_elevation[3:198], rep(0, 196), tolerance = 1e-6)
  }

  # Seen from below, the same motion is clockwise
  flipped <- calculate_kinematics(around_y, vertical = "-y")
  expect_equal(flipped$turning_rate[3:198], rep(-1, 196), tolerance = 1e-3)
})

test_that("a 3D path in the horizontal plane matches its 2D version", {
  t <- seq(0, 2 * pi, length.out = 100)
  flat <- data.frame(time = t, x = cos(t) + t / 3, y = sin(2 * t))

  in_2d <- calculate_kinematics(anicore::as_anipoint(flat))
  in_3d <- calculate_kinematics(
    anicore::as_anipoint(transform(flat, z = 0)),
    vertical = "z"
  )

  for (col in c(
    "course",
    "turning_speed",
    "turning_rate",
    "turning_acceleration",
    "cumulative_turning"
  )) {
    expect_equal(in_3d[[col]], in_2d[[col]], label = col)
  }
  expect_equal(in_3d$course_elevation, rep(0, 100))
})

test_that("course is NA where travel is vertical", {
  data <- data.frame(time = 0:5, x = 0, y = 0, z = c(0, 1, 2, 3, 4, 5)) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data, vertical = "z")

  expect_true(all(is.na(result$course)))
  expect_true(all(is.na(result$turning_rate)))
  expect_equal(result$course_elevation, rep(pi / 2, 6))
  expect_equal(result$turning_speed, rep(0, 6))
})

test_that("cumulative turning counts a turn made while stopped, in 3D", {
  # Up the z axis, a pause, then along +x: a quarter turn
  data <- data.frame(
    time = 0:8,
    x = c(0, 0, 0, 0, 0, 0, 1, 2, 3),
    y = 0,
    z = c(0, 1, 2, 3, 3, 3, 3, 3, 3)
  ) |>
    anicore::as_anipoint()

  result <- calculate_kinematics(data)

  expect_false(anyNA(result$cumulative_turning))
  expect_equal(result$cumulative_turning[1], 0)
  expect_equal(dplyr::last(result$cumulative_turning), pi / 2)
})

test_that("3D angular measures are in the frame's unit_angle", {
  rad <- calculate_kinematics(helix(), vertical = "z")
  deg <- calculate_kinematics(
    anicore::set_metadata(helix(), unit_angle = "deg"),
    vertical = "z"
  )

  for (col in c(
    "course",
    "course_elevation",
    "turning_speed",
    "turning_rate",
    "turning_acceleration",
    "cumulative_turning"
  )) {
    expect_equal(deg[[col]], rad[[col]] * 180 / pi, label = col)
  }
})

test_that("vertical is checked, and ignored outside 3D", {
  expect_error(
    calculate_kinematics(helix(), vertical = "up"),
    "must be"
  )
  expect_error(
    calculate_kinematics(helix(), vertical = c("x", "y")),
    "must be"
  )

  flat <- anicore::as_anipoint(data.frame(
    time = 0:4,
    x = 0:4,
    y = c(0, 1, 0, 1, 0)
  ))
  expect_equal(
    calculate_kinematics(flat, vertical = "x")$course,
    calculate_kinematics(flat)$course
  )
})

test_that("summaries include course and elevation when there is a vertical", {
  result <- summarise_kinematics(calculate_kinematics(helix(), vertical = "z"))

  expect_true(all(
    c(
      "median_course",
      "median_course_elevation",
      "median_turning_rate",
      "median_turning_speed"
    ) %in%
      names(result)
  ))
  expect_equal(result$median_course_elevation, pi / 4, tolerance = 1e-3)
})

test_that("the direction change rate handles one and two observations", {
  expect_equal(direction_change_rate(rbind(c(1, 0, 0)), 0), NA_real_)

  # Two observations: one-sided at both ends, a quarter turn in 2 time units
  v <- rbind(c(1, 0, 0), c(0, 1, 0))
  expect_equal(direction_change_rate(v, c(0, 2)), rep(pi / 4, 2))
})

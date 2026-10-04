# Rediscretising a path to a constant step length, for sinuosity (#104)

test_that("a straight line is cut into equal steps", {
  path <- rediscretise_path(
    data.frame(x = c(0, 10), y = c(0, 0)),
    time = c(0, 10),
    step_length = 2.5
  )

  expect_equal(path$position[, 1], c(0, 2.5, 5, 7.5, 10))
  expect_equal(path$time, c(0, 2.5, 5, 7.5, 10))
  expect_equal(path$run, rep(1L, 5))
})

test_that("a corner is cut where the path leaves the circle", {
  # Along x to (1, 0), then up: the step of length sqrt(2) from the origin
  # lands at (1, 1)
  path <- rediscretise_path(
    data.frame(x = c(0, 1, 1), y = c(0, 0, 3)),
    time = c(0, 1, 4),
    step_length = sqrt(2)
  )

  expect_equal(path$position[2, ], c(1, 1))
  expect_equal(path$time[2], 2)

  turning <- rediscretised_turning(path)
  expect_equal(nrow(turning), 1L)
  # From the diagonal to straight up: 45 degrees
  expect_equal(turning$cos_turning, cos(pi / 4))
})

test_that("jitter within the step gives no steps", {
  set.seed(1)
  jitter <- data.frame(
    x = stats::rnorm(50, sd = 0.01),
    y = stats::rnorm(50, sd = 0.01)
  )
  path <- rediscretise_path(jitter, time = 1:50, step_length = 1)

  expect_equal(nrow(path$position), 1L)
  expect_equal(nrow(rediscretised_turning(path)), 0L)
})

test_that("missing positions break the path, and turning is not measured across", {
  position <- data.frame(
    x = c(0, 1, 2, NA, 2, 2, 2),
    y = c(0, 0, 0, NA, 1, 2, 3)
  )
  path <- rediscretise_path(position, time = 1:7, step_length = 1)

  expect_equal(path$run, c(1L, 1L, 1L, 3L, 3L, 3L))
  turning <- rediscretised_turning(path)
  # Two straight stretches: no turn within either, none counted between
  expect_equal(turning$cos_turning, c(1, 1))
  expect_equal(turning$time, c(2, 6))
})

test_that("the step length is weighted by length, so pauses barely shorten it", {
  moving <- data.frame(x = c(0, 2, 4, 6), y = 0)
  paused <- data.frame(x = c(0, 2, 4, rep(4, 50), 6), y = 0)

  expect_equal(mean_step_length(moving), 2)
  expect_equal(mean_step_length(paused), 2)
  expect_true(is.nan(mean_step_length(data.frame(x = c(1, 1), y = 0))))
})

test_that("no step length, or no complete stretch, gives an empty path", {
  position <- data.frame(x = c(0, 1, NA), y = c(0, 1, NA))
  expect_equal(nrow(rediscretise_path(position, 1:3, 0)$position), 0L)
  expect_equal(nrow(rediscretise_path(position, 1:3, NA_real_)$position), 0L)

  lonely <- data.frame(x = c(0, NA, 1), y = c(0, NA, 1))
  expect_equal(nrow(rediscretise_path(lonely, 1:3, 1)$position), 0L)
})

circle <- function(n = 400, turns = 3) {
  t <- seq(0, 2 * pi * turns, length.out = n)
  data.frame(time = t, x = cos(t), y = sin(t)) |>
    anicore::as_anipoint()
}

# Sinuosity of a circle of radius 1 rediscretised at step r
circle_sinuosity <- function(r) {
  c <- cos(2 * asin(r / 2))
  2 / sqrt(r * (1 + c) / (1 - c))
}

test_that("summarise_path() gives a circle's sinuosity at its mean step", {
  data <- circle()
  step <- mean_step_length(data[c("x", "y")])

  expect_equal(
    summarise_path(data)$sinuosity,
    circle_sinuosity(step),
    tolerance = 1e-3
  )
})

test_that("a stationary stretch leaves summarise_path()'s sinuosity alone", {
  data <- circle()
  set.seed(4)
  # Stop halfway round for 200 frames, jittering
  half <- 200
  still <- data.frame(
    time = data$time[half] + seq_len(200) * 1e-3,
    x = data$x[half] + stats::rnorm(200, sd = 1e-4),
    y = data$y[half] + stats::rnorm(200, sd = 1e-4)
  )
  paused <- dplyr::bind_rows(
    as.data.frame(data)[1:half, c("time", "x", "y")],
    still,
    dplyr::mutate(
      as.data.frame(data)[(half + 1):400, c("time", "x", "y")],
      time = .data$time + 1
    )
  ) |>
    anicore::as_anipoint()

  smooth <- summarise_path(data)
  jittery <- summarise_path(paused)
  legacy <- summarise_path_legacy(paused)

  # Rediscretised, the pause barely moves it; frame by frame, it dominates
  expect_equal(jittery$sinuosity, smooth$sinuosity, tolerance = 0.01)
  expect_gt(abs(legacy$sinuosity - smooth$sinuosity), 1)
})

test_that("add_tortuosity()'s sinuosity is the circle's, and NA at the ends", {
  data <- circle()
  result <- add_tortuosity(data, window_width = 11L)
  step <- mean_step_length(data[c("x", "y")])

  middle <- 20:380
  expect_equal(
    result$sinuosity_11[middle],
    rep(circle_sinuosity(step), length(middle)),
    tolerance = 1e-3
  )
  expect_equal(
    result$e_max_11[middle],
    rep(compute_emax(cos(2 * asin(step / 2))), length(middle)),
    tolerance = 1e-3
  )
  expect_true(all(is.na(result$sinuosity_11[c(1:5, 396:400)])))
})

test_that("a window where the animal moved less than a step has no sinuosity", {
  set.seed(5)
  n <- 60
  # Moving, then still for 30 frames, then moving
  x <- c(0:14, rep(14, 30) + stats::rnorm(30, sd = 1e-3), 15:29)
  data <- data.frame(time = seq_len(n), x = x, y = sin(x)) |>
    anicore::as_anipoint()

  result <- add_tortuosity(data, window_width = 5L)

  expect_true(all(is.na(result$sinuosity_5[25:35])))
  expect_true(all(is.na(result$e_max_5[25:35])))
  expect_false(anyNA(result$sinuosity_5[5:10]))
})

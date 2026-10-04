# A minimum step below which the direction of travel is undefined (#104)

# East for 10 steps, a pause, then north for 10 steps, all with tracking
# noise
pause_and_turn <- function(noise = 0.001) {
  set.seed(104)
  x <- c(0:10, rep(10, 12), rep(10, 10))
  y <- c(rep(0, 11), rep(0, 12), 1:10)
  n <- length(x)
  data.frame(
    time = seq_len(n),
    x = x + stats::rnorm(n, sd = noise),
    y = y + stats::rnorm(n, sd = noise)
  ) |>
    anicore::as_anipoint()
}

test_that("jitter below min_step adds no turning, and the turn is kept", {
  data <- pause_and_turn()

  every <- add_kinematics(data, min_step = 0)
  thresholded <- add_kinematics(data, min_step = 0.1)

  # Every swing of the jitter counted, many full turns' worth
  expect_gt(dplyr::last(every$cumulative_turning), 3 * pi)
  # The quarter turn made during the pause, counted once on moving off, and
  # what the noise adds to the moving steps
  expect_equal(
    dplyr::last(thresholded$cumulative_turning),
    pi / 2,
    tolerance = 0.05
  )
})

test_that("course is NA where the step is below min_step", {
  data <- pause_and_turn()
  result <- add_kinematics(data, min_step = 0.1)
  step <- result$speed * sampling_interval(result$time)

  expect_true(all(is.na(result$course[step < 0.1])))
  expect_false(anyNA(result$course[step >= 0.1]))
  expect_true(all(is.na(result$turning_rate[step < 0.1])))
  # Speed and distance are untouched
  expect_equal(result$speed, add_kinematics(data, min_step = 0)$speed)
})

test_that("in 3D, course needs a horizontal step of min_step", {
  # Climbing fast, with horizontal jitter
  set.seed(1)
  n <- 30
  data <- data.frame(
    time = 1:n,
    x = stats::rnorm(n, sd = 0.001),
    y = stats::rnorm(n, sd = 0.001),
    z = 1:n
  ) |>
    anicore::as_anipoint()

  result <- add_kinematics(data, vertical = "z", min_step = 0.1)

  expect_true(all(is.na(result$course)))
  expect_equal(result$course_elevation, rep(pi / 2, n), tolerance = 1e-3)
  expect_lt(dplyr::last(result$cumulative_turning), 0.1)
})

test_that("summarise_path() passes min_step on to total_turning", {
  data <- pause_and_turn()

  expect_equal(
    summarise_path(data, min_step = 0.1)$total_turning,
    dplyr::last(add_kinematics(data, min_step = 0.1)$cumulative_turning)
  )
  expect_equal(
    summarise_path(data, min_step = 0)$total_turning,
    dplyr::last(add_kinematics(data, min_step = 0)$cumulative_turning)
  )
})

test_that("min_step must be \"auto\" or a non-negative number", {
  data <- pause_and_turn()
  for (bad in list("none", -1, c(1, 2), NA_real_, TRUE)) {
    expect_error(add_kinematics(data, min_step = bad), "min_step")
  }
  expect_error(summarise_path(data, min_step = -1), "min_step")
  expect_no_error(add_kinematics(data, min_step = 1L))
})

# "auto" ---------------------------------------------------------------------

test_that("the positional noise of white noise is its standard deviation", {
  set.seed(2)
  n <- 5000
  position <- data.frame(
    x = stats::rnorm(n, sd = 2),
    y = stats::rnorm(n, sd = 2)
  )
  velocity <- lapply(position, differentiate)

  expect_equal(positional_noise(position, velocity), 2, tolerance = 0.05)
})

test_that("a path turning at constant speed has no positional noise", {
  t <- seq(0, 4 * pi, length.out = 100)
  position <- data.frame(x = cos(t), y = sin(t))
  velocity <- lapply(position, differentiate)

  expect_equal(positional_noise(position, velocity), 0, tolerance = 1e-12)
  expect_equal(
    resolve_min_step("auto", position, velocity, time = t),
    0,
    tolerance = 1e-12
  )
})

test_that("too few rows or no movement give no positional noise", {
  two <- data.frame(x = 1:2, y = 1:2)
  expect_equal(positional_noise(two, two), 0)

  still <- data.frame(x = rep(1, 5), y = rep(1, 5))
  expect_equal(positional_noise(still, lapply(still, differentiate)), 0)
})

test_that("auto is three times the noise, at most half the median step", {
  # Fast and smooth, with a little white noise: three times the noise
  set.seed(3)
  n <- 2000
  t <- seq_len(n)
  position <- data.frame(
    x = t + stats::rnorm(n, sd = 0.05),
    y = stats::rnorm(n, sd = 0.05)
  )
  velocity <- lapply(position, differentiate)
  expect_equal(
    resolve_min_step("auto", position, velocity, time = t),
    3 * positional_noise(position, velocity)
  )

  # A random walk sampled step by step looks like noise: capped
  walk <- data.frame(
    x = cumsum(stats::rnorm(n)),
    y = cumsum(stats::rnorm(n))
  )
  velocity <- lapply(walk, differentiate)
  step <- sqrt(velocity$x^2 + velocity$y^2)
  expect_equal(
    resolve_min_step("auto", walk, velocity, time = t),
    stats::median(step) / 2
  )

  # A number is used as it is
  expect_equal(resolve_min_step(0.5, walk, velocity, time = t), 0.5)
})

test_that("auto removes the turning of a stationary stretch", {
  data <- pause_and_turn()

  auto <- add_kinematics(data)
  expect_equal(
    dplyr::last(auto$cumulative_turning),
    pi / 2,
    tolerance = 0.05
  )
  expect_true(all(is.na(auto$course[14:20])))
})

test_that("auto leaves a smooth path alone", {
  t <- seq(0, 4 * pi, length.out = 200)
  data <- data.frame(time = t, x = cos(t), y = sin(t)) |>
    anicore::as_anipoint()

  expect_equal(add_kinematics(data), add_kinematics(data, min_step = 0))
})

test_that("the sampling interval is half the time between neighbours", {
  expect_equal(sampling_interval(c(0, 1, 3, 4)), c(1, 1.5, 1.5, 1))
})

# The threshold min_step = "auto" chooses (#111)

# Two individuals moving east, pausing, and moving on, with tracking noise,
# one noisier than the other
two_noisy_tracks <- function() {
  set.seed(111)
  x <- c(seq(0, 30, by = 0.5), rep(30, 40), 30 + seq(0.5, 30, by = 0.5))
  n <- length(x)
  sd <- rep(c(0.01, 0.03), each = n)
  data.frame(
    time = rep(seq_len(n), 2),
    individual = rep(c("a", "b"), each = n),
    x = rep(x, 2) + stats::rnorm(2 * n, sd = sd),
    y = stats::rnorm(2 * n, sd = sd)
  ) |>
    anicore::as_anipoint()
}

test_that("compute_min_step() gives one row per trajectory", {
  data <- two_noisy_tracks()
  result <- compute_min_step(data)

  expect_named(result, c("individual", "positional_noise", "min_step"))
  expect_equal(nrow(result), 2L)
  expect_equal(as.character(result$individual), c("a", "b"))
  # The noisier track gets the larger threshold, three times its noise,
  # since both move fast enough that the cap on the median step is far off
  expect_lt(result$min_step[1], result$min_step[2])
  expect_equal(result$min_step, 3 * result$positional_noise)
  expect_equal(result$positional_noise, c(0.01, 0.03), tolerance = 0.15)
})

test_that("the threshold is the one add_kinematics() uses", {
  data <- two_noisy_tracks()
  thresholds <- compute_min_step(data)

  auto <- add_kinematics(data)
  for (i in seq_len(nrow(thresholds))) {
    rows <- data$individual == thresholds$individual[i]
    given <- add_kinematics(data[rows, ], min_step = thresholds$min_step[i])
    expect_equal(auto$course[rows], given$course)
    expect_equal(auto$cumulative_turning[rows], given$cumulative_turning)
  }
  # And it removes the directions of the pause
  expect_false(identical(
    auto$cumulative_turning,
    add_kinematics(data, min_step = 0)$cumulative_turning
  ))
})

test_that("compute_min_step() reports the cap on the median step", {
  # A random walk sampled step by step looks like noise, so the cap sets it
  set.seed(4)
  n <- 500
  walk <- data.frame(
    time = seq_len(n),
    x = cumsum(stats::rnorm(n)),
    y = cumsum(stats::rnorm(n))
  )
  result <- compute_min_step(anicore::as_anipoint(walk))

  velocity <- lapply(walk[c("x", "y")], differentiate)
  step <- sqrt(velocity$x^2 + velocity$y^2)
  expect_equal(result$min_step, stats::median(step) / 2)
  expect_lt(result$min_step, 3 * result$positional_noise)
  expect_equal(
    result$positional_noise,
    positional_noise(walk[c("x", "y")], velocity)
  )
})

test_that("compute_min_step() works in 3D and in any coordinate system", {
  data <- two_noisy_tracks()
  polar <- anispace::map_to_polar(data)
  expect_equal(compute_min_step(polar), compute_min_step(data))

  set.seed(5)
  n <- 200
  data_3d <- data.frame(
    time = seq_len(n),
    x = seq_len(n) / 10 + stats::rnorm(n, sd = 0.01),
    y = stats::rnorm(n, sd = 0.01),
    z = stats::rnorm(n, sd = 0.01)
  ) |>
    anicore::as_anipoint()
  result <- compute_min_step(data_3d)
  expect_equal(nrow(result), 1L)
  expect_equal(result$positional_noise, 0.01, tolerance = 0.15)
})

test_that("compute_min_step() is 0 for a trajectory that never moves", {
  still <- data.frame(time = 1:10, x = 1, y = 2) |>
    anicore::as_anipoint()
  result <- compute_min_step(still)
  expect_equal(result$positional_noise, 0)
  expect_equal(result$min_step, 0)
})

test_that("compute_min_step() checks its input", {
  expect_error(compute_min_step(data.frame(time = 1:3, x = 1:3, y = 1:3)))

  one_d <- data.frame(time = 1:10, x = 1:10) |>
    anicore::as_anipoint()
  expect_error(compute_min_step(one_d), "two or three spatial axes")

  pooled <- suppressWarnings(dplyr::ungroup(two_noisy_tracks()))
  expect_error(compute_min_step(pooled), "one trajectory per group")
})

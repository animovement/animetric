# Derivations need one trajectory per group (#54)
#
# Regrouping an aniframe is allowed, and anicore warns that the grouping and
# the declaration then disagree. These computations make a stronger demand:
# speed and path length come from successive rows, so a group has to hold one
# position per moment or the numbers describe nothing.

spread_keypoints <- function() {
  # Two keypoints 100 apart, each drifting 1 per frame. The true speed of
  # each is 1; pooling them reports 2.7.
  anicore::as_aniframe(
    data.frame(
      individual = "a",
      time = rep(1:5, each = 2),
      keypoint = rep(c("head", "tail"), 5),
      x = as.vector(rbind(1:5, 100 + 1:5)),
      y = 0
    ),
    variables_what = c("individual", "keypoint")
  )
}

pooled <- function(af) {
  suppressWarnings(dplyr::group_by(dplyr::ungroup(af), .data$individual))
}


test_that("the declared grouping gives the true speed", {
  k <- calculate_kinematics(spread_keypoints())

  expect_equal(mean(k$speed, na.rm = TRUE), 1)
})

test_that("pooling trajectories is refused rather than answered wrongly", {
  expect_error(
    calculate_kinematics(pooled(spread_keypoints())),
    "one trajectory per group"
  )
})

test_that("calculate_tortuosity() refuses it too", {
  k <- calculate_kinematics(spread_keypoints())

  expect_error(
    calculate_tortuosity(pooled(k)),
    "one trajectory per group"
  )
})

test_that("summarise_tortuosity() refuses it, since it subtracts along a path", {
  k <- spread_keypoints() |>
    calculate_kinematics() |>
    calculate_tortuosity()

  expect_error(summarise_tortuosity(pooled(k)), "one trajectory per group")
})

test_that("the error says how to summarise more coarsely", {
  err <- tryCatch(
    calculate_kinematics(pooled(spread_keypoints())),
    error = function(e) e
  )

  expect_true(any(grepl("summarise at the declared grouping first", err$body)))
})


# The supported way to summarise more coarsely ----

test_that("summarise at the declared grouping, then combine those results", {
  # Each keypoint's speed is computed on its own trajectory, and the
  # per-animal figure is built from those -- not from a pooled sweep.
  k <- calculate_kinematics(spread_keypoints())

  per_keypoint <- summarise_kinematics(k, measures = "mean_sd")
  per_animal <- dplyr::summarise(
    per_keypoint,
    mean_speed = mean(.data$mean_speed),
    .by = "individual"
  )

  expect_equal(nrow(per_keypoint), 2)
  expect_equal(nrow(per_animal), 1)
  expect_equal(per_animal$mean_speed, 1)
})

test_that("summarise_kinematics() pools freely, having correct values to pool", {
  # No guard here: speed was derived per trajectory, so pooling those values
  # is a choice of statistic rather than a change of computation.
  k <- calculate_kinematics(spread_keypoints())

  expect_no_error(summarise_kinematics(pooled(k)))
  expect_equal(nrow(summarise_kinematics(pooled(k))), 1)
})

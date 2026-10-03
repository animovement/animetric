# Adding a centroid, at whichever level the caller asks for (#47)
#
# The level being collapsed used to be `keypoint`, literally. It is now the
# caller's choice, said either as what collapses (`across`) or what is held
# constant.

custom_identity <- function() {
  anicore::as_anipoint(
    data.frame(
      time = rep(1:4, each = 4),
      animal = rep(rep(c("a1", "a2"), each = 2), 4),
      bodypart = rep(c("head", "tail"), 8),
      x = as.numeric(1:16),
      y = as.numeric(16:1)
    ),
    variables_what = c("animal", "bodypart")
  )
}


# The level it collapses ----

test_that("it collapses the level it is told to", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_point(af, across = "keypoint")

  # One extra row per individual per position, not per keypoint.
  expect_equal(nrow(out), nrow(af) + 2 * 3)
  expect_true("centroid" %in% levels(out$keypoint))
  expect_false("centroid" %in% as.character(out$individual))
})

test_that("across names the level to collapse", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_point(af, across = "individual", name = "group")

  # One extra row per keypoint per position: a centre across the animals.
  expect_equal(nrow(out), nrow(af) + 3 * 3)
  expect_true("group" %in% levels(out$individual))
  expect_false("group" %in% as.character(out$keypoint))
})

test_that("collapsing every level gives one point per position", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_point(af, across = c("individual", "keypoint"), name = "group")

  expect_equal(nrow(out), nrow(af) + 3)
  # The group belongs to no individual and no keypoint, and says so in both.
  expect_true("group" %in% levels(out$individual))
  expect_true("group" %in% levels(out$keypoint))
})

test_that("a frame with several identity variables has to be told which", {
  # `variables_what` is documented coarse to fine, but nothing enforces it
  # and orthogonal attributes do not nest, so the level is not guessed
  # (animovement/anicore#140, animovement/anicore#141).
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_point(af), "has to say which to collapse")
})

test_that("a frame with one identity variable needs no telling", {
  af <- anicore::as_anipoint(
    data.frame(
      time = rep(1:2, each = 3),
      keypoint = rep(c("a", "b", "c"), 2),
      x = as.numeric(1:6),
      y = as.numeric(6:1)
    ),
    variables_what = "keypoint"
  )

  expect_equal(add_point(af), add_point(af, across = "keypoint"))
})

test_that("only identity variables can be collapsed", {
  # Collapsing the index or a temporal variable averages over time, which is
  # what `summarise_*()` does. This one adds a point at each position.
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_point(af, across = "time"), "not an identity variable")
  expect_error(add_point(af, across = "session"), "not an identity variable")
  expect_error(
    add_point(af, across = c("individual", "time")),
    "not an identity variable"
  )
})

test_that("a level that did not vary keeps its value", {
  # Every individual has one strain, so nothing is averaged over strain and
  # calling the result "centroid" there would be a lie.
  af <- anicore::as_anipoint(
    data.frame(
      time = rep(1:2, each = 4),
      strain = rep(c("wild", "mutant"), each = 2, times = 2),
      individual = rep(c("a", "b"), times = 4),
      keypoint = rep(c("head", "tail"), 4),
      x = as.numeric(1:8),
      y = as.numeric(8:1)
    ),
    variables_what = c("strain", "individual", "keypoint")
  )

  out <- add_point(af, across = c("strain", "keypoint"))
  summary_rows <- dplyr::filter(
    dplyr::as_tibble(out),
    .data$keypoint == "centroid"
  )

  expect_false("centroid" %in% as.character(summary_rows$strain))
})


# The identity does not have to be called keypoint (#47) ----

test_that("a frame with its own identity names keeps them", {
  out <- add_point(custom_identity(), across = "bodypart")

  expect_equal(nrow(out), 24)
  expect_true("centroid" %in% levels(out$bodypart))
  expect_equal(anicore::get_variables(out, "what"), c("animal", "bodypart"))
})

test_that("no keypoint column is invented on the way", {
  # `as_anipoint()` re-detecting the declaration injected a default
  # `keypoint` column and stranded it in the result (#47).
  out <- add_point(custom_identity(), across = "bodypart")

  expect_false("keypoint" %in% names(out))
})

test_that("the centroid values are the mean of the members", {
  out <- add_point(custom_identity(), across = "bodypart")
  centroids <- dplyr::filter(
    dplyr::as_tibble(out),
    .data$bodypart == "centroid"
  )

  # a1 at time 1 has head (1, 16) and tail (2, 15).
  first <- dplyr::filter(centroids, .data$animal == "a1", .data$time == 1)
  expect_equal(first$x, 1.5)
  expect_equal(first$y, 15.5)
})


# Types the collapsed column has to survive ----

test_that("an integer identity becomes a factor when collapsed", {
  # `individual` is an integer in example frames, and cannot hold a name.
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 2)
  expect_type(af$individual, "integer")

  out <- add_point(af, across = "individual", name = "group")

  expect_s3_class(out$individual, "factor")
  expect_true("group" %in% levels(out$individual))
})

test_that("a frame without confidence does not gain one", {
  out <- add_point(custom_identity(), across = "bodypart")

  expect_false("confidence" %in% names(out))
})

test_that("a frame with confidence keeps it, NA for the summary", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)
  skip_if_not("confidence" %in% names(af))

  out <- add_point(af, across = "keypoint")
  summary_rows <- dplyr::filter(
    dplyr::as_tibble(out),
    .data$keypoint == "centroid"
  )

  expect_true(all(is.na(summary_rows$confidence)))
})


# Choosing which members take part ----

test_that("include and exclude select the members averaged", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)
  members <- levels(af$keypoint)

  from_two <- add_point(af, across = "keypoint", include = members[1:2])
  without_one <- add_point(af, across = "keypoint", exclude = members[3])

  expect_equal(from_two, without_one)
})

test_that("they need at least two members to average", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_error(
    add_point(af, across = "keypoint", include = levels(af$keypoint)[1]),
    "at least 2 members"
  )
})

test_that("they are refused when several levels are collapsed", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(
    add_point(af, across = c("individual", "keypoint"), include = "head"),
    "name values of one level"
  )
})

test_that("the summary name cannot already be taken", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_error(
    add_point(af, across = "keypoint", name = levels(af$keypoint)[1]),
    "already a value"
  )
})
# The guards on their own ----

test_that("compute_point() refuses include across several levels", {
  # `add_point()` catches this first, so only a direct call reaches it.
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(
    compute_point(
      af,
      across = c("individual", "keypoint"),
      include = "head"
    ),
    "name values of one level"
  )
})

test_that("across has to be column names", {
  af <- anicore::example_anipoint(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_point(af, across = 1), "must name at least one column")
  expect_error(
    add_point(af, across = character(0)),
    "must name at least one column"
  )
})

# Methods (#94) -----------------------------------------------------------

two_points <- function(confidence = c(1, 1, 1, 0)) {
  data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("a", "b"), 2),
    x = c(0, 2, 0, 6),
    y = c(0, 0, 1, 1),
    confidence = confidence
  ) |>
    anicore::as_anipoint()
}

with_yaw <- function(yaw, unit = "rad") {
  data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("a", "b"), 2),
    x = c(0, 2, 0, 2),
    y = 0,
    hd = yaw,
    confidence = c(1, 1, 1, 0)
  ) |>
    anicore::as_anipoint() |>
    anicore::set_variables(
      where = list(position = c(x = "x", y = "y"), orientation = c(yaw = "hd"))
    ) |>
    anicore::set_metadata(unit_angle = unit)
}

three_points <- function() {
  data.frame(
    time = rep(1:2, each = 3),
    keypoint = rep(c("a", "b", "c"), 2),
    x = c(0, 1, 30, 0, 1, 2),
    y = 0
  ) |>
    anicore::as_anipoint()
}

test_that("method = 'median' is robust to a stray point", {
  result <- compute_point(three_points(), method = "median")

  expect_equal(result$x, c(1, 1))
  expect_equal(as.character(result$keypoint), c("median", "median"))
  expect_equal(compute_point(three_points())$x, c(31 / 3, 1))
})

test_that("method = 'weighted' weights by confidence", {
  result <- compute_point(two_points(), method = "weighted")

  expect_equal(result$x, c(1, 0))
  expect_equal(as.character(result$keypoint)[1], "weighted_centroid")
  expect_true(all(is.na(result$confidence)))

  # No weight at all gives NA rather than a division by zero
  none <- compute_point(two_points(c(0, 0, 1, 1)), method = "weighted")
  expect_true(is.na(none$x[1]))
})

test_that("method = 'weighted' needs confidence", {
  expect_error(
    compute_point(three_points(), method = "weighted"),
    "confidence"
  )
})

test_that("a function derives each coordinate, with a name required", {
  result <- compute_point(
    three_points(),
    method = \(x) max(x),
    name = "furthest"
  )
  expect_equal(result$x, c(30, 2))
  expect_equal(as.character(result$keypoint)[1], "furthest")

  expect_error(compute_point(three_points(), method = max), "name.*required")
  expect_error(
    compute_point(three_points(), method = \(x) range(x), name = "r"),
    "single number"
  )
})

test_that("method and name are checked", {
  expect_error(compute_point(three_points(), method = "mean"), "must be")
  expect_error(compute_point(three_points(), name = ""), "non-empty")
  expect_error(compute_point(three_points(), name = c("a", "b")), "non-empty")
})

test_that("a midpoint is the centroid of two members", {
  result <- add_point(
    three_points(),
    include = c("a", "b"),
    name = "ab"
  )
  ab <- dplyr::filter(result, .data$keypoint == "ab")
  expect_equal(ab$x, c(0.5, 0.5))
})

# Orientation (#94) -------------------------------------------------------

test_that("a declared yaw is averaged on the circle", {
  # Facing just either side of pi: the mean is pi, not 0
  result <- compute_point(with_yaw(c(3.1, -3.1, 0.1, 0.3)))
  expect_equal(result$hd, c(pi, 0.2))

  # Weighted by confidence, the second moment takes only the first member's
  weighted <- compute_point(
    with_yaw(c(3.1, -3.1, 0.1, 0.3)),
    method = "weighted"
  )
  expect_equal(weighted$hd[2], 0.1)
})

test_that("the derived yaw keeps the input's range and unit", {
  # 6.0 and 0.1 straddle zero; their mean is just below it, written in
  # [0, 2pi) like the input
  unsigned <- compute_point(with_yaw(c(6.0, 0.1, 1, 1)))
  expect_true(all(unsigned$hd >= 0))
  expect_equal(unsigned$hd[1], (6.0 - 2 * pi + 0.1) / 2 + 2 * pi)

  degrees <- compute_point(with_yaw(c(350, -10, 90, 90), unit = "deg"))
  expect_equal(degrees$hd, c(-10, 90))
})

test_that("cancelling directions give NA", {
  expect_true(is.na(compute_point(with_yaw(c(0, pi, 1, 1)))$hd[1]))
})

test_that("a declared quaternion is averaged in 3D", {
  half <- 0.2
  d <- data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("a", "b"), 2),
    x = c(0, 2, 0, 2),
    y = 0,
    z = 0,
    qw = c(1, cos(half), 1, NA),
    qx = 0,
    qy = 0,
    qz = c(0, sin(half), 0, NA)
  )
  af <- anicore::as_anipoint(d) |>
    anicore::set_variables(
      where = list(
        position = c(x = "x", y = "y", z = "z"),
        orientation = c(qw = "qw", qx = "qx", qy = "qy", qz = "qz")
      )
    )

  result <- compute_point(af)
  expected <- anispace::quat_mean(rbind(
    c(1, 0, 0, 0),
    c(cos(half), 0, 0, sin(half))
  ))
  expect_equal(
    unname(unlist(result[1, c("qw", "qx", "qy", "qz")])),
    as.vector(expected)
  )
  # One member missing its orientation: the other's
  expect_equal(
    unname(unlist(result[2, c("qw", "qx", "qy", "qz")])),
    c(1, 0, 0, 0)
  )
})

test_that("add_point() keeps the orientation declaration", {
  result <- add_point(with_yaw(c(0.1, 0.3, 0.1, 0.3)))
  expect_equal(
    anicore::get_variables(result, "where", "orientation"),
    c(yaw = "hd")
  )
  centroid <- dplyr::filter(result, .data$keypoint == "centroid")
  expect_equal(centroid$hd, c(0.2, 0.2))
})

# Deprecated add_centroid() and compute_centroid() ------------------------

test_that("add_centroid() and compute_centroid() are deprecated, unchanged", {
  af <- with_yaw(c(0.1, 0.3, 0.1, 0.3))

  expect_warning(
    added <- add_centroid(af),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    computed <- compute_centroid(af),
    class = "lifecycle_warning_deprecated"
  )

  # As before: the centroid's position, with orientation left NA
  centroid <- dplyr::filter(added, .data$keypoint == "centroid")
  expect_equal(centroid$x, c(1, 1))
  expect_true(all(is.na(centroid$hd)))
  expect_equal(computed$x, c(1, 1))
  expect_false("hd" %in% names(computed))
})

test_that("quaternions are weighted by confidence, and NA when none is known", {
  half <- 0.2
  d <- data.frame(
    time = rep(1:2, each = 2),
    keypoint = rep(c("a", "b"), 2),
    x = c(0, 2, 0, 2),
    y = 0,
    z = 0,
    qw = c(1, cos(half), NA, NA),
    qx = 0,
    qy = 0,
    qz = c(0, sin(half), NA, NA),
    confidence = c(0, 1, 1, 1)
  )
  af <- anicore::as_anipoint(d) |>
    anicore::set_variables(
      where = list(
        position = c(x = "x", y = "y", z = "z"),
        orientation = c(qw = "qw", qx = "qx", qy = "qy", qz = "qz")
      )
    )

  result <- compute_point(af, method = "weighted")

  # Only the second member carries weight at the first moment
  expect_equal(
    unname(unlist(result[1, c("qw", "qx", "qy", "qz")])),
    c(cos(half), 0, 0, sin(half))
  )
  expect_true(all(is.na(unlist(result[2, c("qw", "qx", "qy", "qz")]))))
})

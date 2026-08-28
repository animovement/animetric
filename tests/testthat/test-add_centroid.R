# Adding a centroid, at whichever level the caller asks for (#47)
#
# The level being collapsed used to be `keypoint`, literally. It is now the
# caller's choice, said either as what collapses (`across`) or what is held
# constant.

custom_identity <- function() {
  anicore::as_aniframe(
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
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_centroid(af, across = "keypoint")

  # One extra row per individual per position, not per keypoint.
  expect_equal(nrow(out), nrow(af) + 2 * 3)
  expect_true("centroid" %in% levels(out$keypoint))
  expect_false("centroid" %in% as.character(out$individual))
})

test_that("across names the level to collapse", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_centroid(af, across = "individual", name = "group")

  # One extra row per keypoint per position: a centre across the animals.
  expect_equal(nrow(out), nrow(af) + 3 * 3)
  expect_true("group" %in% levels(out$individual))
  expect_false("group" %in% as.character(out$keypoint))
})

test_that("collapsing every level gives one point per position", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  out <- add_centroid(af, across = c("individual", "keypoint"), name = "group")

  expect_equal(nrow(out), nrow(af) + 3)
  # The group belongs to no individual and no keypoint, and says so in both.
  expect_true("group" %in% levels(out$individual))
  expect_true("group" %in% levels(out$keypoint))
})

test_that("a frame with several identity variables has to be told which", {
  # `variables_what` is documented coarse to fine, but nothing enforces it
  # and orthogonal attributes do not nest, so the level is not guessed
  # (animovement/anicore#140, animovement/anicore#141).
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_centroid(af), "has to say which to collapse")
})

test_that("a frame with one identity variable needs no telling", {
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 3),
      keypoint = rep(c("a", "b", "c"), 2),
      x = as.numeric(1:6),
      y = as.numeric(6:1)
    ),
    variables_what = "keypoint"
  )

  expect_equal(add_centroid(af), add_centroid(af, across = "keypoint"))
})

test_that("only identity variables can be collapsed", {
  # Collapsing the index or a temporal variable averages over time, which is
  # what `summarise_*()` does. This one adds a point at each position.
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_centroid(af, across = "time"), "not an identity variable")
  expect_error(add_centroid(af, across = "session"), "not an identity variable")
  expect_error(
    add_centroid(af, across = c("individual", "time")),
    "not an identity variable"
  )
})

test_that("a level that did not vary keeps its value", {
  # Every individual has one strain, so nothing is averaged over strain and
  # calling the result "centroid" there would be a lie.
  af <- anicore::as_aniframe(
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

  out <- add_centroid(af, across = c("strain", "keypoint"))
  summary_rows <- dplyr::filter(
    dplyr::ungroup(out),
    .data$keypoint == "centroid"
  )

  expect_false("centroid" %in% as.character(summary_rows$strain))
})


# The identity does not have to be called keypoint (#47) ----

test_that("a frame with its own identity names keeps them", {
  out <- add_centroid(custom_identity(), across = "bodypart")

  expect_equal(nrow(out), 24)
  expect_true("centroid" %in% levels(out$bodypart))
  expect_equal(anicore::get_variables_what(out), c("animal", "bodypart"))
})

test_that("no keypoint column is invented on the way", {
  # `as_aniframe()` re-detecting the declaration injected a default
  # `keypoint` column and stranded it in the result (#47).
  out <- add_centroid(custom_identity(), across = "bodypart")

  expect_false("keypoint" %in% names(out))
})

test_that("the centroid values are the mean of the members", {
  out <- add_centroid(custom_identity(), across = "bodypart")
  centroids <- dplyr::filter(dplyr::ungroup(out), .data$bodypart == "centroid")

  # a1 at time 1 has head (1, 16) and tail (2, 15).
  first <- dplyr::filter(centroids, .data$animal == "a1", .data$time == 1)
  expect_equal(first$x, 1.5)
  expect_equal(first$y, 15.5)
})


# Types the collapsed column has to survive ----

test_that("an integer identity becomes a factor when collapsed", {
  # `individual` is an integer in example frames, and cannot hold a name.
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 2)
  expect_type(af$individual, "integer")

  out <- add_centroid(af, across = "individual", name = "group")

  expect_s3_class(out$individual, "factor")
  expect_true("group" %in% levels(out$individual))
})

test_that("a frame without confidence does not gain one", {
  out <- add_centroid(custom_identity(), across = "bodypart")

  expect_false("confidence" %in% names(out))
})

test_that("a frame with confidence keeps it, NA for the summary", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
  skip_if_not("confidence" %in% names(af))

  out <- add_centroid(af, across = "keypoint")
  summary_rows <- dplyr::filter(
    dplyr::ungroup(out),
    .data$keypoint == "centroid"
  )

  expect_true(all(is.na(summary_rows$confidence)))
})


# Choosing which members take part ----

test_that("include and exclude select the members averaged", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
  members <- levels(af$keypoint)

  from_two <- add_centroid(af, across = "keypoint", include = members[1:2])
  without_one <- add_centroid(af, across = "keypoint", exclude = members[3])

  expect_equal(from_two, without_one)
})

test_that("they need at least two members to average", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_error(
    add_centroid(af, across = "keypoint", include = levels(af$keypoint)[1]),
    "at least 2 members"
  )
})

test_that("they are refused when several levels are collapsed", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(
    add_centroid(af, across = c("individual", "keypoint"), include = "head"),
    "name values of one level"
  )
})

test_that("the summary name cannot already be taken", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_error(
    add_centroid(af, across = "keypoint", name = levels(af$keypoint)[1]),
    "already a value"
  )
})


# The old name ----

test_that("summarise_keypoints() still works, deprecated", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  expect_warning(out <- summarise_keypoints(af), "deprecated")
  expect_equal(out, add_centroid(af, across = "keypoint"))
})

test_that("its keypoints argument maps onto include", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)
  members <- levels(af$keypoint)

  expect_warning(
    out <- summarise_keypoints(af, keypoints = members[1:2]),
    "deprecated"
  )
  expect_equal(
    out,
    add_centroid(af, across = "keypoint", include = members[1:2])
  )
})


# The guards on their own ----

test_that("compute_centroid() refuses include across several levels", {
  # `add_centroid()` catches this first, so only a direct call reaches it.
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(
    compute_centroid(
      af,
      across = c("individual", "keypoint"),
      include = "head"
    ),
    "name values of one level"
  )
})

test_that("across has to be column names", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 2, n_keypoints = 3)

  expect_error(add_centroid(af, across = 1), "must name at least one column")
  expect_error(
    add_centroid(af, across = character(0)),
    "must name at least one column"
  )
})

test_that("the deprecated name needs an identity to collapse", {
  af <- suppressWarnings(anicore::as_aniframe(
    data.frame(time = 1:3, x = 1:3, y = 1:3),
    variables_what = character(0)
  ))

  expect_error(
    suppressWarnings(summarise_keypoints)(af),
    "declares no identity variables"
  )
})

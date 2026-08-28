# Identity, position and the index come from the declaration (#47)

custom_af <- function() {
  anicore::as_aniframe(
    data.frame(
      time = rep(1:3, each = 2),
      animal = rep("a1", 6),
      bodypart = rep(c("head", "tail"), 3),
      x = c(0, 10, 0, 10, 0, 10),
      y = c(0, 0, 2, 2, 4, 4)
    ),
    variables_what = c("animal", "bodypart")
  )
}

test_that("compute_centroid() works on a frame with its own identity names", {
  out <- compute_centroid(custom_af())

  expect_equal(as.character(unique(out$bodypart)), "centroid")
  expect_equal(out$x, c(5, 5, 5))
  expect_equal(out$y, c(0, 2, 4))
})

test_that("compute_centroid() keeps the declaration rather than re-detecting", {
  # Detection only recognises the standard identity names, so re-detecting
  # would inject a `keypoint` column and replace the declaration.
  out <- compute_centroid(custom_af())

  expect_equal(anicore::get_variables_what(out), c("animal", "bodypart"))
  expect_false("keypoint" %in% names(out))
})

test_that("compute_centroid() collapses only the finest identity", {
  af <- anicore::as_aniframe(
    data.frame(
      time = rep(1:2, each = 4),
      animal = rep(c("a1", "a1", "a2", "a2"), 2),
      bodypart = rep(c("head", "tail"), 4),
      x = c(0, 10, 100, 110, 0, 10, 100, 110),
      y = 0
    ),
    variables_what = c("animal", "bodypart")
  )

  out <- compute_centroid(af)

  # One centroid per animal per timepoint, not one overall.
  expect_equal(nrow(out), 4)
  expect_setequal(as.character(unique(out$animal)), c("a1", "a2"))
  expect_setequal(out$x, c(5, 105))
})

test_that("summarise_keypoints() selects on the declared identity", {
  out <- summarise_keypoints(custom_af(), name = "mid")

  expect_true("mid" %in% as.character(out$bodypart))
  expect_equal(nrow(out), 9)
})

test_that("a frame with no identity variables is refused by name", {
  af <- suppressWarnings(anicore::as_aniframe(
    data.frame(time = 1:3, x = 1:3, y = 1:3),
    variables_what = character(0)
  ))

  expect_error(compute_centroid(af), "declares no identity variables")
})

test_that("the standard keypoint frame is unaffected", {
  af <- anicore::example_aniframe(n_obs = 3, n_individuals = 1, n_keypoints = 3)

  out <- compute_centroid(af)

  expect_equal(as.character(unique(out$keypoint)), "centroid")
  expect_equal(nrow(out), 3)
})

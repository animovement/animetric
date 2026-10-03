# add_orientation() declares orientation from points (#97)

body_2d <- function() {
  data.frame(
    time = rep(1:2, each = 4),
    keypoint = rep(c("tail", "head", "eye_right", "eye_left"), 2),
    x = c(0, 1, 1, 1, 0, 0, 1, -1),
    y = c(0, 0, -1, 1, 0, 1, 1, 1)
  ) |>
    anicore::as_anipoint()
}

# The orientation at each moment, which every member shares
heading_at <- function(result, col = "heading") {
  vapply(
    sort(unique(result$time)),
    \(t) unique(result[[col]][result$time == t]),
    numeric(1)
  )
}

test_that("2D: heading is the direction from -> to, declared as yaw", {
  result <- add_orientation(body_2d(), from = "tail", to = "head")

  expect_equal(heading_at(result), c(0, pi / 2))
  expect_equal(
    anicore::get_variables(result, "where", "orientation"),
    c(yaw = "heading")
  )
  expect_s3_class(result, "anipoint")
})

test_that("2D: across the body, to is on the left", {
  # Right eye to left eye runs along +y at the first moment, so forward is +x;
  # at the second it runs along -x, so forward is +y
  result <- add_orientation(
    body_2d(),
    from = "eye_right",
    to = "eye_left",
    perpendicular = TRUE
  )
  expect_equal(heading_at(result), c(0, pi / 2))
})

test_that("heading is in the frame's unit_angle", {
  deg <- anicore::set_metadata(body_2d(), unit_angle = "deg")
  expect_equal(heading_at(add_orientation(deg, "tail", "head")), c(0, 90))
})

test_that("attach_to writes only the named members, and calls build up", {
  head <- add_orientation(
    body_2d(),
    from = "eye_right",
    to = "eye_left",
    perpendicular = TRUE,
    attach_to = c("eye_right", "eye_left")
  )
  eyes <- head$keypoint %in% c("eye_right", "eye_left")
  expect_true(all(is.na(head$heading[!eyes])))
  expect_false(anyNA(head$heading[eyes]))

  # A second call fills the rest of the same column
  both <- add_orientation(head, "tail", "head", attach_to = c("tail", "head"))
  expect_false(anyNA(both$heading))
  expect_equal(both$heading[eyes], head$heading[eyes])
})

test_that("existing values are protected unless overwrite = TRUE", {
  once <- add_orientation(body_2d(), "tail", "head")

  expect_error(add_orientation(once, "eye_right", "head"), "overwrite = TRUE")
  again <- add_orientation(once, "head", "tail", overwrite = TRUE)
  expect_equal(heading_at(again), c(pi, -pi / 2))
})

test_that("name defaults to the declared columns, and is checked", {
  declared <- add_orientation(body_2d(), "tail", "head", name = "body_yaw")
  expect_true("body_yaw" %in% names(declared))
  expect_false("heading" %in% names(declared))

  # Writing somewhere else needs overwrite, and then re-declares
  expect_error(
    add_orientation(
      declared,
      "tail",
      "head",
      name = "other",
      attach_to = "tail"
    ),
    "already declares an orientation"
  )
  moved <- add_orientation(
    declared,
    "tail",
    "head",
    name = "other",
    overwrite = TRUE
  )
  expect_equal(
    anicore::get_variables(moved, "where", "orientation"),
    c(yaw = "other")
  )

  expect_error(
    add_orientation(body_2d(), "tail", "head", name = c("a", "b")),
    "distinct"
  )
})

test_that("missing or coincident points give NA", {
  data <- body_2d()
  data$x[data$keypoint == "head" & data$time == 2] <- NA
  missing <- add_orientation(data, "tail", "head")
  expect_true(all(is.na(missing$heading[missing$time == 2])))

  # The tail and head at the same place at the first moment
  data <- body_2d()
  data$x[data$keypoint == "head" & data$time == 1] <- 0
  same <- add_orientation(data, "tail", "head")
  expect_true(all(is.na(same$heading[same$time == 1])))
  expect_false(anyNA(same$heading[same$time == 2]))
})

test_that("each subject gets its own orientation", {
  two <- dplyr::bind_rows(
    dplyr::mutate(as.data.frame(body_2d()), individual = "a"),
    dplyr::mutate(
      as.data.frame(body_2d()),
      individual = "b",
      x = -x
    )
  ) |>
    anicore::as_anipoint()

  result <- add_orientation(two, "tail", "head", level = "keypoint")
  a <- result$heading[result$individual == "a" & result$time == 1]
  b <- result$heading[result$individual == "b" & result$time == 1]
  expect_equal(unique(a), 0)
  expect_equal(unique(b), pi)
})

test_that("arguments are checked", {
  af <- body_2d()
  expect_error(add_orientation(af, "tail", "nose"), "not a member")
  expect_error(add_orientation(af, c("tail", "head"), "head"), "single member")
  expect_error(
    add_orientation(af, "tail", "head", attach_to = "nose"),
    "not a member"
  )
  expect_error(
    add_orientation(af, "tail", "head", attach_to = 1),
    "name members"
  )
  expect_error(
    add_orientation(af, "tail", "head", plane = "eye_left"),
    "only used in 3D"
  )
  expect_error(add_orientation(af, "tail", "head", perpendicular = NA), "TRUE")
  expect_error(add_orientation(af, "tail", "head", overwrite = "yes"), "TRUE")

  one_d <- anicore::as_anipoint(data.frame(time = 1:2, x = 1:2))
  expect_error(add_orientation(one_d, "centroid", "centroid"), "2D or 3D")

  multi <- anicore::example_anipoint(
    n_obs = 2,
    n_individuals = 2,
    n_keypoints = 2
  )
  expect_error(add_orientation(multi, "head", "neck"), "across")
  expect_error(
    add_orientation(multi, "head", "neck", level = c("individual", "keypoint")),
    "one identity variable"
  )
})

# 3D ---------------------------------------------------------------------

body_3d <- function() {
  # A rigid body turned by a known rotation
  r <- anispace::quat_from_axis_angle(c(1, 2, 3), 0.9)
  local <- rbind(
    tail = c(0, 0, 0),
    head = c(2, 0, 0),
    left = c(0.5, 1, 0),
    eye_right = c(1.8, -0.3, 0.2),
    eye_left = c(1.8, 0.3, 0.2),
    nose = c(2.3, 0, 0.2)
  )
  world <- anispace::quat_rotate(r, local)
  list(
    rotation = r,
    frame = anicore::as_anipoint(data.frame(
      time = 1,
      keypoint = rownames(local),
      x = world[, 1],
      y = world[, 2],
      z = world[, 3]
    ))
  )
}

test_that("3D: from, to and a plane point recover the body's rotation", {
  body <- body_3d()
  result <- add_orientation(body$frame, "tail", "head", plane = "left")

  q <- as.matrix(result[1, c("qw", "qx", "qy", "qz")])
  expect_equal(anispace::quat_distance(q, body$rotation), 0, tolerance = 1e-12)
  expect_equal(
    anicore::get_variables(result, "where", "orientation"),
    c(qw = "qw", qx = "qx", qy = "qy", qz = "qz")
  )
})

test_that("3D: across the eyes, with the nose fixing which way is forward", {
  body <- body_3d()
  result <- add_orientation(
    body$frame,
    from = "eye_right",
    to = "eye_left",
    perpendicular = TRUE,
    plane = "nose"
  )
  q <- as.matrix(result[1, c("qw", "qx", "qy", "qz")])

  # The eyes are level and the nose is ahead of them, so this is the body's
  # own orientation
  expect_equal(anispace::quat_distance(q, body$rotation), 0, tolerance = 1e-12)
})

test_that("3D needs a plane point", {
  expect_error(add_orientation(body_3d()$frame, "tail", "head"), "fix the roll")
})

test_that("aligning by the declared orientation agrees with aligning on points", {
  af <- add_orientation(body_2d(), "tail", "head")

  by_points <- anispace::transform_to_egocentric(
    af,
    to = "tail",
    align = c("tail", "head"),
    level = "keypoint"
  )
  by_orientation <- anispace::transform_to_egocentric(
    af,
    to = "tail",
    align = "orientation",
    level = "keypoint"
  )
  expect_equal(by_orientation$x, by_points$x, tolerance = 1e-12)
  expect_equal(by_orientation$y, by_points$y, tolerance = 1e-12)
})

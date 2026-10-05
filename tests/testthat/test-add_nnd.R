# test-add_nnd.R
# Tests:
# - Returns aniframe with correct new columns (2D)
# - Returns aniframe with correct new columns (3D)
# - Calculates correct nearest neighbour distances
# - Identifies correct nearest neighbour individual
# - Filters neighbours by the neighbour argument
# - Returns nnd_1_keypoint column when keypoint values are non-NA
# - Handles n > 1 for second nearest individual
# - Returns NA when no neighbours available (all same individual)
# - Returns NA when all individuals are NA
# - Returns NA when not enough individuals for n
# - Errors when all individuals are NA
# - Errors when neighbour names a keypoint column that is absent
# - Errors when no requested keypoints are present in data
# - Warns when some requested keypoints are not present in data
# - Groups correctly by session/trial/time
# - Handles several neighbour keypoints
# - Maintains incoming classes and columns

test_that("returns aniframe with correct new columns (2D)", {
  data <- anicore::anipoint(
    time = c(1, 1, 2, 2),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 10),
    y = c(0, 0, 0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_s3_class(result, "aniframe")
  expect_true("nnd_1_distance" %in% names(result))
  expect_true("nnd_1_individual" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("returns aniframe with correct new columns (3D)", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0),
    z = c(0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_s3_class(result, "aniframe")
  expect_true("nnd_1_distance" %in% names(result))
  expect_true("nnd_1_individual" %in% names(result))
})

test_that("calculates correct nearest neighbour distances (2D)", {
  data <- anicore::anipoint(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- add_nnd(data, across = "individual")

  # Individual 1 -> nearest is 2 at distance 10
  # Individual 2 -> nearest is 1 at distance 10
  # Individual 3 -> nearest is 2 at distance 15
  expect_equal(result$nnd_1_distance[result$individual == "1"], 10)
  expect_equal(result$nnd_1_distance[result$individual == "2"], 10)
  expect_equal(result$nnd_1_distance[result$individual == "3"], 15)
})

test_that("calculates correct nearest neighbour distances (3D)", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 3),
    y = c(0, 4),
    z = c(0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_equal(result$nnd_1_distance, c(5, 5))
})

test_that("identifies correct nearest neighbour individual", {
  data <- anicore::anipoint(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 100),
    y = c(0, 0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_equal(
    as.character(result$nnd_1_individual[result$individual == "1"]),
    "2"
  )
  expect_equal(
    as.character(result$nnd_1_individual[result$individual == "2"]),
    "1"
  )
  expect_equal(
    as.character(result$nnd_1_individual[result$individual == "3"]),
    "2"
  )
})

test_that("filters neighbours by the neighbour argument", {
  data <- anicore::anipoint(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 10, 12),
    y = c(0, 0, 0, 0)
  )

  result <- add_nnd(
    data,
    across = "individual",
    neighbour = list(keypoint = "nose")
  )

  # Individual 1's nose (x=0) -> nearest nose is individual 2's nose (x=10), distance 10
  # Individual 1's tail (x=5) -> nearest nose is individual 2's nose (x=10), distance 5
  expect_equal(
    result$nnd_1_distance[result$individual == "1" & result$keypoint == "nose"],
    10
  )
  expect_equal(
    result$nnd_1_distance[result$individual == "1" & result$keypoint == "tail"],
    5
  )
  expect_equal(
    as.character(result$nnd_1_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "nose"
  )
})

test_that("returns nnd_1_keypoint column when keypoint values are non-NA", {
  data <- anicore::anipoint(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 3, 100),
    y = c(0, 0, 0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_true("nnd_1_keypoint" %in% names(result))
  # Individual 1's nose (x=0) is closest to individual 2's nose (x=3)
  expect_equal(
    as.character(result$nnd_1_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "nose"
  )
})

test_that("handles n > 1 for second nearest individual", {
  data <- anicore::anipoint(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- add_nnd(data, across = "individual", n = 2L)

  # Individual 1 -> 2nd nearest individual is 3 at distance 25
  # Individual 2 -> 2nd nearest individual is 3 at distance 15
  # Individual 3 -> 2nd nearest individual is 1 at distance 25
  expect_equal(result$nnd_2_distance[result$individual == "1"], 25)
  expect_equal(result$nnd_2_distance[result$individual == "2"], 15)
  expect_equal(result$nnd_2_distance[result$individual == "3"], 25)

  expect_equal(
    as.character(result$nnd_2_individual[result$individual == "1"]),
    "3"
  )
  expect_equal(
    as.character(result$nnd_2_individual[result$individual == "2"]),
    "3"
  )
  expect_equal(
    as.character(result$nnd_2_individual[result$individual == "3"]),
    "1"
  )
})

test_that("n = 2 finds second nearest individual, not second nearest point", {
  # Individual 2 has two keypoints, both closer than individual 3
  # n = 2 should return individual 3, not individual 2's second keypoint
  data <- anicore::anipoint(
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 2, 3),
    keypoint = c("nose", "nose", "tail", "nose"),
    x = c(0, 5, 7, 100),
    y = c(0, 0, 0, 0)
  )

  result <- add_nnd(data, across = "individual", n = 2L)

  # Individual 1's nose: nearest ind is 2 (dist 5), 2nd nearest is 3 (dist 100)
  ind1_row <- result$individual == "1" & result$keypoint == "nose"
  expect_equal(as.character(result$nnd_2_individual[ind1_row]), "3")
  expect_equal(result$nnd_2_distance[ind1_row], 100)
})

test_that("returns NA when no neighbours available (all same individual)", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- add_nnd(data, across = "individual")

  expect_true(all(is.na(result$nnd_1_distance)))
  expect_true(all(is.na(result$nnd_1_individual)))
})

test_that("returns NA when not enough individuals for n", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- add_nnd(data, across = "individual", n = 2L)

  expect_true(all(is.na(result$nnd_2_distance)))
})

test_that("errors when the column named by `across` is absent", {
  data <- anicore::anipoint(
    time = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    add_nnd(data, across = "individual"),
    "must name a single column present in the data"
  )
  # Reading an absent column would warn on the way to the error.
  expect_no_warning(try(
    add_nnd(data, across = "individual"),
    silent = TRUE
  ))
})

test_that("errors when all individuals are NA", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(NA, NA),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(add_nnd(data, across = "individual"), "only .*NA.* values")
})

test_that("errors when neighbour names a column that is absent", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    add_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "nose")
    ),
    "not found in the data"
  )
  expect_no_warning(
    try(
      add_nnd(
        data,
        across = "individual",
        neighbour = list(keypoint = "nose")
      ),
      silent = TRUE
    )
  )
})

test_that("a frame without keypoints computes distances without warning", {
  # aniframe stopped adding a phantom `keypoint` beside an existing
  # identity, so probing the column directly warned on every call.
  data <- anicore::anipoint(
    time = c(1, 1, 2, 2),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 20),
    y = c(0, 0, 0, 0)
  )

  expect_no_warning(result <- add_nnd(data, across = "individual"))
  expect_true("nnd_1_distance" %in% names(result))
})

test_that("errors when no requested keypoints are present in data", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    add_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "left_ear")
    )
  )
})

test_that("warns when some requested keypoints are not present in data", {
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_warning(
    add_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = c("nose", "left_ear"))
    ),
    "absent from"
  )
})

test_that("groups correctly by session/trial/time", {
  data <- anicore::anipoint(
    session = c(1, 1, 2, 2),
    trial = c(1, 1, 1, 1),
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 100),
    y = c(0, 0, 0, 0)
  )

  result <- add_nnd(data, across = "individual")

  # Session 1: distance is 10
  # Session 2: distance is 100
  expect_equal(
    result$nnd_1_distance[result$session == "1" & result$individual == "1"],
    10
  )
  expect_equal(
    result$nnd_1_distance[result$session == "2" & result$individual == "1"],
    100
  )
})

test_that("handles several neighbour keypoints", {
  data <- anicore::anipoint(
    time = c(1, 1, 1, 1, 1, 1),
    individual = c(1, 1, 1, 2, 2, 2),
    keypoint = c(
      "nose",
      "left_ear",
      "right_ear",
      "nose",
      "left_ear",
      "right_ear"
    ),
    x = c(0, 1, 2, 10, 11, 8),
    y = c(0, 0, 0, 0, 0, 0)
  )

  result <- add_nnd(
    data,
    across = "individual",
    neighbour = list(keypoint = c("left_ear", "right_ear"))
  )

  # Individual 1's nose (x=0) -> nearest ear of ind 2 is right_ear (x=8), distance 8
  expect_equal(
    result$nnd_1_distance[result$individual == "1" & result$keypoint == "nose"],
    8
  )
  expect_equal(
    as.character(result$nnd_1_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "right_ear"
  )
})

test_that("errors when the frame declares no temporal context", {
  # The index counts as context here (anicore#109), so reaching this branch
  # means a frame whose index column is gone as well as its temporal keys.
  data <- anicore::anipoint(
    individual = c(1, 2),
    time = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  ) |>
    anicore::set_variables(when = character(0))
  data <- drop_column_unchecked(data, "time")

  expect_error(add_nnd(data, across = "individual"), "context")
})

test_that("Maintains incoming classes", {
  data <- anicore::example_anipoint() |>
    add_kinematics() |>
    add_nnd(across = "individual")

  expect_s3_class(data, "anipoint")
  expect_true("speed" %in% names(data))
})

# ---- Explicit variable roles (#37) --------------------------------------

pair_af <- function() {
  # A: nose at 0, tail at 10.  B: nose at 30, tail at 12.
  anicore::anipoint(
    individual = c("A", "A", "B", "B"),
    keypoint = c("nose", "tail", "nose", "tail"),
    time = rep(1, 4),
    x = c(0, 10, 30, 12),
    y = rep(0, 4)
  )
}

test_that("neighbours are not matched across observations", {
  # The reprex from #37: `observation` joined variables_when in aniframe
  # 0.6.0, but the hard-coded context list never picked it up, so clips
  # were pooled and each animal was matched to one in another clip.
  af <- anicore::anipoint(
    observation = rep(c("clip_a", "clip_b"), each = 2),
    individual = rep(c(1L, 2L), 2),
    time = rep(1, 4),
    x = c(0, 100, 0, 1),
    y = rep(0, 4)
  )

  result <- add_nnd(af, across = "individual")
  clip_a <- result[result$observation == "clip_a", ]

  expect_equal(sort(clip_a$nnd_1_distance), c(100, 100))
})

test_that("focal and neighbour can name different keypoints", {
  result <- add_nnd(
    pair_af(),
    across = "individual",
    focal = list(keypoint = "nose"),
    neighbour = list(keypoint = "tail")
  )

  noses <- result[result$keypoint == "nose", ]
  expect_equal(noses$nnd_1_distance[noses$individual == "A"], 12)
  expect_equal(noses$nnd_1_distance[noses$individual == "B"], 20)
  expect_true(all(as.character(noses$nnd_1_keypoint) == "tail"))

  # Points outside `focal` are not measured from.
  expect_true(all(is.na(result$nnd_1_distance[result$keypoint == "tail"])))
})

test_that("across = keypoint measures between points, and within keeps it inside the animal", {
  free <- add_nnd(pair_af(), across = "keypoint")
  inside <- add_nnd(pair_af(), across = "keypoint", within = "individual")

  # Unconstrained, B's tail finds A's nose (12) rather than its own (18).
  b_tail <- free$individual == "B" & free$keypoint == "tail"
  expect_equal(free$nnd_1_distance[b_tail], 12)
  expect_equal(as.character(free$nnd_1_individual[b_tail]), "A")

  b_tail <- inside$individual == "B" & inside$keypoint == "tail"
  expect_equal(inside$nnd_1_distance[b_tail], 18)
})

test_that("within pairs like with like", {
  result <- add_nnd(pair_af(), across = "individual", within = "keypoint")

  noses <- result[result$keypoint == "nose", ]
  tails <- result[result$keypoint == "tail", ]
  expect_true(all(noses$nnd_1_distance == 30))
  expect_true(all(tails$nnd_1_distance == 2))
})

test_that("a frame identified by track works", {
  af <- anicore::anipoint(
    track = c(1L, 2L),
    time = c(1, 1),
    x = c(0, 5),
    y = c(0, 0)
  )

  result <- add_nnd(af, across = "track")
  expect_true("nnd_1_track" %in% names(result))
  expect_equal(result$nnd_1_distance, c(5, 5))
})

test_that("non-Cartesian coordinates error with a pointer to the conversion", {
  af <- anicore::anipoint(
    individual = c(1L, 2L),
    time = c(1, 1),
    rho = c(1, 2),
    phi = c(0, pi)
  )

  expect_error(add_nnd(af, across = "individual"), "Cartesian")
  expect_error(add_nnd(af, across = "individual"), "map_to_cartesian")
})

test_that("keypoint_neighbour is gone from add_nnd()", {
  expect_error(
    add_nnd(pair_af(), across = "individual", keypoint_neighbour = "tail"),
    "unused argument"
  )
})

test_that("focal and neighbour must be named lists", {
  expect_error(
    add_nnd(pair_af(), across = "individual", focal = "nose"),
    "named list"
  )
})

test_that("within must name existing columns", {
  expect_error(
    add_nnd(pair_af(), across = "individual", within = "nope"),
    "must name a single column"
  )
})

test_that("one-dimensional data errors rather than measuring in a line", {
  af <- anicore::anipoint(
    individual = c(1L, 2L),
    time = c(1, 1),
    x = c(0, 5)
  )

  expect_error(
    add_nnd(af, across = "individual"),
    "two spatial variables"
  )
})

test_that("a neighbour restriction matching no rows errors", {
  # The column is present but carries no usable value, so nothing can
  # satisfy the restriction. Under the old API this was a keypoint-shaped
  # special case; it is now the general "nothing matches" error.
  data <- anicore::anipoint(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c(NA, NA),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    add_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "nose")
    ),
    "No rows match"
  )
})

test_that("add_nnd() keeps the input's metadata and declaration", {
  af <- anicore::example_anipoint(
    n_obs = 5,
    n_individuals = 3,
    n_keypoints = 1
  ) |>
    anicore::set_metadata(sampling_rate = 30, source = "test")
  out <- add_nnd(af, across = "individual")
  expect_equal(anicore::get_metadata(out, "sampling_rate"), 30)
  expect_equal(anicore::get_metadata(out, "source"), "test")
  expect_equal(anicore::get_keys(out), anicore::get_keys(af))
})

test_that("add_nnd() works with renamed axis columns", {
  af <- anicore::example_anipoint(
    n_obs = 5,
    n_individuals = 3,
    n_keypoints = 1
  ) |>
    dplyr::rename(u = x, v = y) |>
    anicore::set_variables(where = c(x = "u", y = "v"))
  out <- add_nnd(af, across = "individual")
  expect_equal(anicore::get_axes(out), c(x = "u", y = "v"))
  expect_true("nnd_1_individual" %in% names(out))
})

test_that("every added column carries the neighbour rank", {
  data <- pair_af()
  out <- add_nnd(data, across = "individual")

  expect_equal(
    setdiff(names(out), names(data)),
    c("nnd_1_individual", "nnd_1_keypoint", "nnd_1_distance")
  )
})

test_that("calls with different n sit side by side", {
  data <- anicore::anipoint(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  both <- data |>
    add_nnd(across = "individual", n = 1) |>
    add_nnd(across = "individual", n = 2)

  expect_equal(
    setdiff(names(both), names(data)),
    c(
      "nnd_1_individual",
      "nnd_1_distance",
      "nnd_2_individual",
      "nnd_2_distance"
    )
  )
  expect_equal(both$nnd_1_distance, c(10, 10, 15))
  expect_equal(both$nnd_2_distance, c(25, 15, 25))
  expect_equal(
    both$nnd_1_distance,
    add_nnd(data, across = "individual", n = 1)$nnd_1_distance
  )
  expect_s3_class(both, "anipoint")
})

test_that("the names split on the rank, underscores and all", {
  af <- anicore::anipoint(
    track_id = c("a", "b"),
    time = c(1, 1),
    x = c(0, 5),
    y = c(0, 0)
  )

  added <- setdiff(names(add_nnd(af, across = "track_id", n = 12)), names(af))
  # anipoint() adds a `keypoint` column, which is left unconstrained
  expect_equal(
    added,
    c("nnd_12_track_id", "nnd_12_keypoint", "nnd_12_distance")
  )

  pattern <- "^nnd_(\\d+)_(.+)$"
  expect_true(all(grepl(pattern, added)))
  expect_equal(sub(pattern, "\\1", added), rep("12", 3))
  expect_equal(
    sub(pattern, "\\2", added),
    c("track_id", "keypoint", "distance")
  )
  # And back again
  expect_equal(
    paste0("nnd_", sub(pattern, "\\1", added), "_", sub(pattern, "\\2", added)),
    added
  )
})

test_that("a repeated call with the same n errors helpfully", {
  data <- add_nnd(pair_af(), across = "individual")

  expect_error(
    add_nnd(data, across = "individual"),
    "already has.*nnd_1_individual"
  )
  expect_no_error(add_nnd(data, across = "individual", n = 2))
})

test_that("n must be a single whole number of 1 or more", {
  for (bad in list(0, 1.5, -1, c(1, 2), NA_integer_, "1")) {
    expect_error(add_nnd(pair_af(), across = "individual", n = bad), "`n`")
  }
})

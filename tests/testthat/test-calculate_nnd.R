# test-calculate_nnd.R
# Tests:
# - Returns aniframe with correct new columns (2D)
# - Returns aniframe with correct new columns (3D)
# - Calculates correct nearest neighbour distances
# - Identifies correct nearest neighbour individual
# - Filters neighbours by keypoint_neighbour parameter
# - Returns nnd_keypoint column when keypoint values are non-NA
# - Handles n > 1 for second nearest individual
# - Returns NA when no neighbours available (all same individual)
# - Returns NA when all individuals are NA
# - Returns NA when not enough individuals for n
# - Errors when all individuals are NA
# - Errors when keypoint_neighbour specified but all keypoints are NA
# - Errors when no requested keypoints are present in data
# - Warns when some requested keypoints are not present in data
# - Groups correctly by session/trial/time
# - Handles vector of keypoint_neighbour values
# - Maintains aniframe_kin class

test_that("returns aniframe with correct new columns (2D)", {
  data <- anicore::aniframe(
    time = c(1, 1, 2, 2),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 10),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_s3_class(result, "aniframe")
  expect_true("nnd_distance" %in% names(result))
  expect_true("nnd_individual" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("returns aniframe with correct new columns (3D)", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0),
    z = c(0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_s3_class(result, "aniframe")
  expect_true("nnd_distance" %in% names(result))
  expect_true("nnd_individual" %in% names(result))
})

test_that("calculates correct nearest neighbour distances (2D)", {
  data <- anicore::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  # Individual 1 -> nearest is 2 at distance 10
  # Individual 2 -> nearest is 1 at distance 10
  # Individual 3 -> nearest is 2 at distance 15
  expect_equal(result$nnd_distance[result$individual == "1"], 10)
  expect_equal(result$nnd_distance[result$individual == "2"], 10)
  expect_equal(result$nnd_distance[result$individual == "3"], 15)
})

test_that("calculates correct nearest neighbour distances (3D)", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 3),
    y = c(0, 4),
    z = c(0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_equal(result$nnd_distance, c(5, 5))
})

test_that("identifies correct nearest neighbour individual", {
  data <- anicore::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 100),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_equal(
    as.character(result$nnd_individual[result$individual == "1"]),
    "2"
  )
  expect_equal(
    as.character(result$nnd_individual[result$individual == "2"]),
    "1"
  )
  expect_equal(
    as.character(result$nnd_individual[result$individual == "3"]),
    "2"
  )
})

test_that("filters neighbours by keypoint_neighbour parameter", {
  data <- anicore::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 10, 12),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(
    data,
    across = "individual",
    neighbour = list(keypoint = "nose")
  )

  # Individual 1's nose (x=0) -> nearest nose is individual 2's nose (x=10), distance 10
  # Individual 1's tail (x=5) -> nearest nose is individual 2's nose (x=10), distance 5
  expect_equal(
    result$nnd_distance[result$individual == "1" & result$keypoint == "nose"],
    10
  )
  expect_equal(
    result$nnd_distance[result$individual == "1" & result$keypoint == "tail"],
    5
  )
  expect_equal(
    as.character(result$nnd_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "nose"
  )
})

test_that("returns nnd_keypoint column when keypoint values are non-NA", {
  data <- anicore::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 3, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_true("nnd_keypoint" %in% names(result))
  # Individual 1's nose (x=0) is closest to individual 2's nose (x=3)
  expect_equal(
    as.character(result$nnd_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "nose"
  )
})

test_that("handles n > 1 for second nearest individual", {
  data <- anicore::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual", n = 2L)

  # Individual 1 -> 2nd nearest individual is 3 at distance 25
  # Individual 2 -> 2nd nearest individual is 3 at distance 15
  # Individual 3 -> 2nd nearest individual is 1 at distance 25
  expect_equal(result$nnd_distance[result$individual == "1"], 25)
  expect_equal(result$nnd_distance[result$individual == "2"], 15)
  expect_equal(result$nnd_distance[result$individual == "3"], 25)

  expect_equal(
    as.character(result$nnd_individual[result$individual == "1"]),
    "3"
  )
  expect_equal(
    as.character(result$nnd_individual[result$individual == "2"]),
    "3"
  )
  expect_equal(
    as.character(result$nnd_individual[result$individual == "3"]),
    "1"
  )
})

test_that("n = 2 finds second nearest individual, not second nearest point", {
  # Individual 2 has two keypoints, both closer than individual 3
  # n = 2 should return individual 3, not individual 2's second keypoint
  data <- anicore::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 2, 3),
    keypoint = c("nose", "nose", "tail", "nose"),
    x = c(0, 5, 7, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual", n = 2L)

  # Individual 1's nose: nearest ind is 2 (dist 5), 2nd nearest is 3 (dist 100)
  ind1_row <- result$individual == "1" & result$keypoint == "nose"
  expect_equal(as.character(result$nnd_individual[ind1_row]), "3")
  expect_equal(result$nnd_distance[ind1_row], 100)
})

test_that("returns NA when no neighbours available (all same individual)", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  expect_true(all(is.na(result$nnd_distance)))
  expect_true(all(is.na(result$nnd_individual)))
})

test_that("returns NA when not enough individuals for n", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- calculate_nnd(data, across = "individual", n = 2L)

  expect_true(all(is.na(result$nnd_distance)))
})

test_that("errors when the column named by `across` is absent", {
  data <- anicore::aniframe(
    time = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    calculate_nnd(data, across = "individual"),
    "must name a single column present in the data"
  )
  # Reading an absent column would warn on the way to the error.
  expect_no_warning(try(
    calculate_nnd(data, across = "individual"),
    silent = TRUE
  ))
})

test_that("errors when all individuals are NA", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(NA, NA),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(calculate_nnd(data, across = "individual"), "only .*NA.* values")
})

test_that("errors when keypoint_neighbour is given but the column is absent", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    calculate_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "nose")
    ),
    "not found in the data"
  )
  expect_no_warning(
    try(
      calculate_nnd(
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
  data <- anicore::aniframe(
    time = c(1, 1, 2, 2),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 20),
    y = c(0, 0, 0, 0)
  )

  expect_no_warning(result <- calculate_nnd(data, across = "individual"))
  expect_true("nnd_distance" %in% names(result))
})

test_that("errors when no requested keypoints are present in data", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    calculate_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "left_ear")
    )
  )
})

test_that("warns when some requested keypoints are not present in data", {
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_warning(
    calculate_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = c("nose", "left_ear"))
    ),
    "absent from"
  )
})

test_that("groups correctly by session/trial/time", {
  data <- anicore::aniframe(
    session = c(1, 1, 2, 2),
    trial = c(1, 1, 1, 1),
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, across = "individual")

  # Session 1: distance is 10
  # Session 2: distance is 100
  expect_equal(
    result$nnd_distance[result$session == "1" & result$individual == "1"],
    10
  )
  expect_equal(
    result$nnd_distance[result$session == "2" & result$individual == "1"],
    100
  )
})

test_that("handles vector of keypoint_neighbour values", {
  data <- anicore::aniframe(
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

  result <- calculate_nnd(
    data,
    across = "individual",
    neighbour = list(keypoint = c("left_ear", "right_ear"))
  )

  # Individual 1's nose (x=0) -> nearest ear of ind 2 is right_ear (x=8), distance 8
  expect_equal(
    result$nnd_distance[result$individual == "1" & result$keypoint == "nose"],
    8
  )
  expect_equal(
    as.character(result$nnd_keypoint[
      result$individual == "1" & result$keypoint == "nose"
    ]),
    "right_ear"
  )
})

test_that("errors when the frame declares no temporal context", {
  # The index counts as context here (anicore#109), and `get_index()` falls
  # back to `time` whatever the field says -- so reaching this branch means
  # a frame whose index column is gone as well as its temporal variables.
  data <- anicore::aniframe(
    individual = c(1, 2),
    time = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )
  md <- anicore::get_metadata(data)
  md$variables_when <- character(0)
  attr(data, "metadata") <- md
  data <- suppressWarnings(dplyr::select(dplyr::ungroup(data), -"time"))

  expect_error(calculate_nnd(data, across = "individual"), "context")
})

test_that("Maintains incoming classes", {
  data <- anicore::example_aniframe() |>
    calculate_kinematics() |>
    calculate_nnd(across = "individual")

  expect_contains(class(data), "aniframe_kin")
})

# ---- Explicit variable roles (#37) --------------------------------------

pair_af <- function() {
  # A: nose at 0, tail at 10.  B: nose at 30, tail at 12.
  anicore::aniframe(
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
  af <- anicore::aniframe(
    observation = rep(c("clip_a", "clip_b"), each = 2),
    individual = rep(c(1L, 2L), 2),
    time = rep(1, 4),
    x = c(0, 100, 0, 1),
    y = rep(0, 4)
  )

  result <- calculate_nnd(af, across = "individual")
  clip_a <- result[result$observation == "clip_a", ]

  expect_equal(sort(clip_a$nnd_distance), c(100, 100))
})

test_that("focal and neighbour can name different keypoints", {
  result <- calculate_nnd(
    pair_af(),
    across = "individual",
    focal = list(keypoint = "nose"),
    neighbour = list(keypoint = "tail")
  )

  noses <- result[result$keypoint == "nose", ]
  expect_equal(noses$nnd_distance[noses$individual == "A"], 12)
  expect_equal(noses$nnd_distance[noses$individual == "B"], 20)
  expect_true(all(as.character(noses$nnd_keypoint) == "tail"))

  # Points outside `focal` are not measured from.
  expect_true(all(is.na(result$nnd_distance[result$keypoint == "tail"])))
})

test_that("across = keypoint measures between points, and within keeps it inside the animal", {
  free <- calculate_nnd(pair_af(), across = "keypoint")
  inside <- calculate_nnd(pair_af(), across = "keypoint", within = "individual")

  # Unconstrained, B's tail finds A's nose (12) rather than its own (18).
  b_tail <- free$individual == "B" & free$keypoint == "tail"
  expect_equal(free$nnd_distance[b_tail], 12)
  expect_equal(as.character(free$nnd_individual[b_tail]), "A")

  b_tail <- inside$individual == "B" & inside$keypoint == "tail"
  expect_equal(inside$nnd_distance[b_tail], 18)
})

test_that("within pairs like with like", {
  result <- calculate_nnd(pair_af(), across = "individual", within = "keypoint")

  noses <- result[result$keypoint == "nose", ]
  tails <- result[result$keypoint == "tail", ]
  expect_true(all(noses$nnd_distance == 30))
  expect_true(all(tails$nnd_distance == 2))
})

test_that("a frame identified by track works", {
  af <- anicore::aniframe(
    track = c(1L, 2L),
    time = c(1, 1),
    x = c(0, 5),
    y = c(0, 0)
  )

  result <- calculate_nnd(af, across = "track")
  expect_true("nnd_track" %in% names(result))
  expect_equal(result$nnd_distance, c(5, 5))
})

test_that("non-Cartesian coordinates error with a pointer to the conversion", {
  af <- anicore::aniframe(
    individual = c(1L, 2L),
    time = c(1, 1),
    rho = c(1, 2),
    phi = c(0, pi)
  )

  expect_error(calculate_nnd(af, across = "individual"), "Cartesian")
  expect_error(calculate_nnd(af, across = "individual"), "map_to_cartesian")
})

test_that("keypoint_neighbour is deprecated but still works", {
  expect_warning(
    result <- calculate_nnd(
      pair_af(),
      across = "individual",
      keypoint_neighbour = "tail"
    ),
    "deprecated"
  )

  expect_true(all(as.character(result$nnd_keypoint) == "tail"))
})

test_that("focal and neighbour must be named lists", {
  expect_error(
    calculate_nnd(pair_af(), across = "individual", focal = "nose"),
    "named list"
  )
})

test_that("within must name existing columns", {
  expect_error(
    calculate_nnd(pair_af(), across = "individual", within = "nope"),
    "must name a single column"
  )
})

test_that("one-dimensional data errors rather than measuring in a line", {
  af <- anicore::aniframe(
    individual = c(1L, 2L),
    time = c(1, 1),
    x = c(0, 5)
  )

  expect_error(
    calculate_nnd(af, across = "individual"),
    "two spatial variables"
  )
})

test_that("a neighbour restriction matching no rows errors", {
  # The column is present but carries no usable value, so nothing can
  # satisfy the restriction. Under the old API this was a keypoint-shaped
  # special case; it is now the general "nothing matches" error.
  data <- anicore::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c(NA, NA),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    calculate_nnd(
      data,
      across = "individual",
      neighbour = list(keypoint = "nose")
    ),
    "No rows match"
  )
})

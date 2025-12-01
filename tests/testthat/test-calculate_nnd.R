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

test_that("returns aniframe with correct new columns (2D)", {
  data <- aniframe::aniframe(
    time = c(1, 1, 2, 2),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 10),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data)

  expect_s3_class(result, "aniframe")
  expect_true("nnd_distance" %in% names(result))
  expect_true("nnd_individual" %in% names(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("returns aniframe with correct new columns (3D)", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0),
    z = c(0, 0)
  )

  result <- calculate_nnd(data)

  expect_s3_class(result, "aniframe")
  expect_true("nnd_distance" %in% names(result))
  expect_true("nnd_individual" %in% names(result))
})

test_that("calculates correct nearest neighbour distances (2D)", {
  data <- aniframe::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data)

  # Individual 1 -> nearest is 2 at distance 10
  # Individual 2 -> nearest is 1 at distance 10
  # Individual 3 -> nearest is 2 at distance 15
  expect_equal(result$nnd_distance[result$individual == "1"], 10)
  expect_equal(result$nnd_distance[result$individual == "2"], 10)
  expect_equal(result$nnd_distance[result$individual == "3"], 15)
})

test_that("calculates correct nearest neighbour distances (3D)", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 3),
    y = c(0, 4),
    z = c(0, 0)
  )

  result <- calculate_nnd(data)

  expect_equal(result$nnd_distance, c(5, 5))
})

test_that("identifies correct nearest neighbour individual", {
  data <- aniframe::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 100),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data)

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
  data <- aniframe::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 10, 12),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, keypoint_neighbour = "nose")

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
  data <- aniframe::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 1, 2, 2),
    keypoint = c("nose", "tail", "nose", "tail"),
    x = c(0, 5, 3, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data)

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
  data <- aniframe::aniframe(
    time = c(1, 1, 1),
    individual = c(1, 2, 3),
    x = c(0, 10, 25),
    y = c(0, 0, 0)
  )

  result <- calculate_nnd(data, n = 2L)

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
  data <- aniframe::aniframe(
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 2, 3),
    keypoint = c("nose", "nose", "tail", "nose"),
    x = c(0, 5, 7, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data, n = 2L)

  # Individual 1's nose: nearest ind is 2 (dist 5), 2nd nearest is 3 (dist 100)
  ind1_row <- result$individual == "1" & result$keypoint == "nose"
  expect_equal(as.character(result$nnd_individual[ind1_row]), "3")
  expect_equal(result$nnd_distance[ind1_row], 100)
})

test_that("returns NA when no neighbours available (all same individual)", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- calculate_nnd(data)

  expect_true(all(is.na(result$nnd_distance)))
  expect_true(all(is.na(result$nnd_individual)))
})

test_that("returns NA when not enough individuals for n", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  result <- calculate_nnd(data, n = 2L)

  expect_true(all(is.na(result$nnd_distance)))
})

test_that("errors when all individuals are NA", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(calculate_nnd(data), "individual")
})

test_that("errors when keypoint_neighbour specified but all keypoints are NA", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(calculate_nnd(data, keypoint_neighbour = "nose"), "keypoint")
})

test_that("errors when no requested keypoints are present in data", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_error(
    calculate_nnd(data, keypoint_neighbour = "left_ear")
  )
})

test_that("warns when some requested keypoints are not present in data", {
  data <- aniframe::aniframe(
    time = c(1, 1),
    individual = c(1, 2),
    keypoint = c("nose", "tail"),
    x = c(0, 10),
    y = c(0, 0)
  )

  expect_warning(
    calculate_nnd(data, keypoint_neighbour = c("nose", "left_ear")),
    "not present"
  )
})

test_that("groups correctly by session/trial/time", {
  data <- aniframe::aniframe(
    session = c(1, 1, 2, 2),
    trial = c(1, 1, 1, 1),
    time = c(1, 1, 1, 1),
    individual = c(1, 2, 1, 2),
    x = c(0, 10, 0, 100),
    y = c(0, 0, 0, 0)
  )

  result <- calculate_nnd(data)

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
  data <- aniframe::aniframe(
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

  result <- calculate_nnd(data, keypoint_neighbour = c("left_ear", "right_ear"))

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

test_that("errors when no context columns present", {
  # Create a mock aniframe without session, trial, or time
  data <- data.frame(
    individual = factor(c(1, 2)),
    keypoint = factor(c(NA, NA)),
    x = c(0, 10),
    y = c(0, 0),
    confidence = c(NA_real_, NA_real_)
  )
  class(data) <- c("aniframe", "tbl_df", "tbl", "data.frame")

  expect_error(calculate_nnd(data), "context column")
})

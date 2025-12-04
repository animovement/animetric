# test-compute_nnd.R
# Tests:
# - Returns tibble with correct columns (2D, no keypoints)
# - Returns tibble with correct columns (2D, with keypoints)
# - Returns tibble with correct columns (3D)
# - Calculates correct Euclidean distances (2D)
# - Calculates correct Euclidean distances (3D)
# - Excludes same-individual points from distance calculation
# - Filters candidates by keypoint_neighbour
# - Returns NA distance when no valid neighbours
# - Returns NA distance when not enough individuals for n
# - Correctly identifies nth nearest individual
# - n = 2 finds second nearest individual, not second nearest point
# - Preserves factor type in individual output
# - Preserves factor type in keypoint output
# - Handles empty keypoint_neighbour filter gracefully
# - Handles NA individuals correctly

test_that("returns tibble with correct columns (2D, no keypoints)", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 2))
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("nnd_individual", "nnd_distance"))
  expect_equal(nrow(result), 2)
})

test_that("returns tibble with correct columns (2D, with keypoints)", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 2)),
    keypoint = factor(c("nose", "nose"))
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("nnd_individual", "nnd_keypoint", "nnd_distance"))
})

test_that("returns tibble with correct columns (3D)", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    z = c(0, 0),
    individual = factor(c(1, 2))
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("nnd_individual", "nnd_distance"))
})

test_that("calculates correct Euclidean distances (2D)", {
  result <- compute_nnd(
    x = c(0, 3),
    y = c(0, 4),
    individual = factor(c(1, 2))
  )

  expect_equal(result$nnd_distance, c(5, 5))
})

test_that("calculates correct Euclidean distances (3D)", {
  result <- compute_nnd(
    x = c(0, 2),
    y = c(0, 3),
    z = c(0, 6),
    individual = factor(c(1, 2))
  )

  # sqrt(4 + 9 + 36) = sqrt(49) = 7
  expect_equal(result$nnd_distance, c(7, 7))
})

test_that("excludes same-individual points from distance calculation", {
  result <- compute_nnd(
    x = c(0, 1, 10),
    y = c(0, 0, 0),
    individual = factor(c(1, 1, 2))
  )

  # Points 1 & 2 (ind 1): nearest other individual is ind 2 at x=10
  # Point 1 (x=0): distance to ind 2 is 10
  # Point 2 (x=1): distance to ind 2 is 9
  # Point 3 (ind 2, x=10): nearest ind 1 point is at x=1, distance 9
  expect_equal(result$nnd_distance, c(10, 9, 9))
  expect_equal(as.character(result$nnd_individual), c("2", "2", "1"))
})

test_that("finds closest point on neighbouring individual", {
  # Individual 2 has two points, should find the closest one
  result <- compute_nnd(
    x = c(0, 10, 5),
    y = c(0, 0, 0),
    individual = factor(c(1, 2, 2))
  )

  # Individual 1 (x=0): closest point of ind 2 is at x=5, distance 5
  expect_equal(result$nnd_distance[1], 5)
})

test_that("filters candidates by keypoint_neighbour", {
  result <- compute_nnd(
    x = c(0, 10, 100),
    y = c(0, 0, 0),
    individual = factor(c(1, 2, 2)),
    keypoint = factor(c("nose", "nose", "tail")),
    keypoint_neighbour = "tail"
  )

  # Individual 1's nose can only consider individual 2's tail (x=100)
  expect_equal(result$nnd_distance[1], 100)
  expect_equal(as.character(result$nnd_keypoint[1]), "tail")
})

test_that("returns NA distance when no valid neighbours (same individual)", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 1))
  )

  expect_true(all(is.na(result$nnd_distance)))
  expect_true(all(is.na(result$nnd_individual)))
})

test_that("returns NA distance when not enough individuals for n", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 2)),
    n = 2L
  )

  expect_true(all(is.na(result$nnd_distance)))
})

test_that("correctly identifies nth nearest individual", {
  result <- compute_nnd(
    x = c(0, 10, 30),
    y = c(0, 0, 0),
    individual = factor(c(1, 2, 3)),
    n = 2L
  )

  # Individual 1: 2nd nearest is 3 at distance 30
  # Individual 2: 2nd nearest is 3 at distance 20
  # Individual 3: 2nd nearest is 1 at distance 30
  expect_equal(result$nnd_distance, c(30, 20, 30))
  expect_equal(as.character(result$nnd_individual), c("3", "3", "1"))
})

test_that("n = 2 finds second nearest individual, not second nearest point", {
  # Individual 2 has two points, both closer than individual 3
  # n = 2 should still return individual 3
  result <- compute_nnd(
    x = c(0, 5, 7, 100),
    y = c(0, 0, 0, 0),
    individual = factor(c(1, 2, 2, 3))
  )

  result_n2 <- compute_nnd(
    x = c(0, 5, 7, 100),
    y = c(0, 0, 0, 0),
    individual = factor(c(1, 2, 2, 3)),
    n = 2L
  )

  # n = 1: Individual 1's nearest is ind 2 at distance 5
  expect_equal(result$nnd_distance[1], 5)
  expect_equal(as.character(result$nnd_individual[1]), "2")

  # n = 2: Individual 1's 2nd nearest is ind 3 at distance 100 (not ind 2's other point)
  expect_equal(result_n2$nnd_distance[1], 100)
  expect_equal(as.character(result_n2$nnd_individual[1]), "3")
})

test_that("handles single point per individual", {
  result <- compute_nnd(
    x = c(0, 5, 15),
    y = c(0, 0, 0),
    individual = factor(c(1, 2, 3))
  )

  expect_equal(result$nnd_distance, c(5, 5, 10))
  expect_equal(as.character(result$nnd_individual), c("2", "1", "2"))
})

test_that("preserves factor type in individual output", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c("a", "b"))
  )

  expect_s3_class(result$nnd_individual, "factor")
  expect_equal(levels(result$nnd_individual), c("a", "b"))
})

test_that("preserves factor type in keypoint output", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 2)),
    keypoint = factor(c("nose", "tail"))
  )

  expect_s3_class(result$nnd_keypoint, "factor")
})

test_that("handles empty keypoint_neighbour filter gracefully", {
  result <- compute_nnd(
    x = c(0, 10),
    y = c(0, 0),
    individual = factor(c(1, 2)),
    keypoint = factor(c("nose", "tail")),
    keypoint_neighbour = "nonexistent"
  )

  expect_true(all(is.na(result$nnd_distance)))
})

test_that("handles NA individuals correctly", {
  result <- compute_nnd(
    x = c(0, 10, 20),
    y = c(0, 0, 0),
    individual = factor(c(NA, 1, 2))
  )

  # NA individual should get NA result
  expect_true(is.na(result$nnd_distance[1]))

  # Non-NA individuals should find each other
  expect_equal(result$nnd_distance[2], 10)
  expect_equal(result$nnd_distance[3], 10)
})

test_that("returns closest point on nth nearest individual with keypoints", {
  # Individual 2 has nose at x=10 and tail at x=15
  # Individual 3 has nose at x=100
  # For individual 1, 2nd nearest individual is 3
  result <- compute_nnd(
    x = c(0, 10, 15, 100),
    y = c(0, 0, 0, 0),
    individual = factor(c(1, 2, 2, 3)),
    keypoint = factor(c("nose", "nose", "tail", "nose")),
    n = 2L
  )

  # Individual 1: 2nd nearest individual is 3, closest point is nose at x=100
  expect_equal(result$nnd_distance[1], 100)
  expect_equal(as.character(result$nnd_individual[1]), "3")
  expect_equal(as.character(result$nnd_keypoint[1]), "nose")
})

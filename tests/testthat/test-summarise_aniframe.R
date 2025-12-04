# Tests for summarise_aniframe and join_summaries
#
# summarise_aniframe:
# - summarise_aniframe returns combined output with default type
# - summarise_aniframe returns only kinematics when type = "kinematics"
# - summarise_aniframe returns only tortuosity when type = "tortuosity"
# - summarise_aniframe passes measures argument to summarise_kinematics
# - summarise_aniframe preserves grouping structure
# - summarise_aniframe works with 2D data
# - summarise_aniframe works with 3D data
# - summarise_aniframe validates input
#
# join_summaries:
# - join_summaries returns single summary unchanged
# - join_summaries binds columns when no groups
# - join_summaries left joins when groups present
# - join_summaries handles multiple summaries correctly

# Helper to create mock 2D kinematics aniframe
mock_kin_2d <- function(n = 10, grouped = FALSE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  aniframe::as_aniframe(data) |>
    calculate_kinematics()
}

# Helper to create mock 3D kinematics aniframe
mock_kin_3d <- function(n = 10, grouped = FALSE) {
  data <- data.frame(
    time = seq_len(n),
    x = cumsum(rnorm(n)),
    y = cumsum(rnorm(n)),
    z = cumsum(rnorm(n))
  )

  if (grouped) {
    data <- rbind(
      transform(data, individual = "a"),
      transform(data, individual = "b")
    )
  }

  aniframe::as_aniframe(data) |>
    calculate_kinematics()
}


# summarise_aniframe: type argument --------------------------------------

test_that("summarise_aniframe returns combined output with default type", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data)

  # Should have columns from both kinematics and tortuosity
  expect_true("median_speed" %in% names(result))
  expect_true("total_path_length" %in% names(result))
  expect_true("straightness" %in% names(result))
})

test_that("summarise_aniframe returns only kinematics when type = 'kinematics'", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, type = "kinematics")

  expect_true("median_speed" %in% names(result))
  expect_false("total_path_length" %in% names(result))
  expect_false("straightness" %in% names(result))
})

test_that("summarise_aniframe returns only tortuosity when type = 'tortuosity'", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, type = "tortuosity")

  expect_false("median_speed" %in% names(result))
  expect_true("total_path_length" %in% names(result))
  expect_true("straightness" %in% names(result))
})

test_that("summarise_aniframe accepts multiple type values", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data, type = c("tortuosity", "kinematics"))

  expect_true("median_speed" %in% names(result))
  expect_true("total_path_length" %in% names(result))
})


# summarise_aniframe: measures argument ----------------------------------

test_that("summarise_aniframe passes measures argument correctly", {
  data <- mock_kin_2d()

  result_median <- summarise_aniframe(
    data,
    type = "kinematics",
    measures = "median_mad"
  )
  result_mean <- summarise_aniframe(
    data,
    type = "kinematics",
    measures = "mean_sd"
  )

  expect_true("median_speed" %in% names(result_median))
  expect_false("mean_speed" %in% names(result_median))

  expect_true("mean_speed" %in% names(result_mean))
  expect_false("median_speed" %in% names(result_mean))
})


# summarise_aniframe: grouping -------------------------------------------

test_that("summarise_aniframe preserves grouping structure", {
  data <- mock_kin_2d(grouped = TRUE)
  result <- summarise_aniframe(data)

  expect_equal(nrow(result), 2L)
  expect_true("individual" %in% names(result))
})

test_that("summarise_aniframe grouped output has correct columns", {
  data <- mock_kin_2d(grouped = TRUE)
  result <- summarise_aniframe(data)

  # Should have id column plus columns from both summary types

  expect_true("individual" %in% names(result))
  expect_true("median_speed" %in% names(result))
  expect_true("total_path_length" %in% names(result))
})


# summarise_aniframe: 2D vs 3D -------------------------------------------

test_that("summarise_aniframe works with 2D data", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data)

  # 2D should have angular columns
  expect_true("median_angular_speed" %in% names(result))
  expect_true("total_angular_path_length" %in% names(result))
})

test_that("summarise_aniframe works with 3D data", {
  data <- mock_kin_3d()
  result <- summarise_aniframe(data)

  # 3D should not have angular columns
  expect_false("median_angular_speed" %in% names(result))
  expect_false("total_angular_path_length" %in% names(result))

  # But should have the basic columns

  expect_true("median_speed" %in% names(result))
  expect_true("total_path_length" %in% names(result))
})


# summarise_aniframe: output structure -----------------------------------

test_that("summarise_aniframe returns one row for ungrouped data", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data)

  expect_equal(nrow(result), 1L)
})

test_that("summarise_aniframe returns data.frame", {
  data <- mock_kin_2d()
  result <- summarise_aniframe(data)

  expect_s3_class(result, "data.frame")
})


# join_summaries ---------------------------------------------------------

test_that("join_summaries returns single summary unchanged", {
  summary <- data.frame(a = 1, b = 2)
  summaries <- list(only = summary)

  result <- join_summaries(summaries, character(0))

  expect_identical(result, summary)
})

test_that("join_summaries binds columns when no groups", {
  summary1 <- data.frame(a = 1, b = 2)
  summary2 <- data.frame(c = 3, d = 4)
  summaries <- list(first = summary1, second = summary2)

  result <- join_summaries(summaries, character(0))

  expect_equal(ncol(result), 4L)
  expect_equal(names(result), c("a", "b", "c", "d"))
  expect_equal(nrow(result), 1L)
})

test_that("join_summaries left joins when groups present", {
  summary1 <- data.frame(id = c("a", "b"), x = c(1, 2))
  summary2 <- data.frame(id = c("a", "b"), y = c(3, 4))
  summaries <- list(first = summary1, second = summary2)

  result <- join_summaries(summaries, "id")

  expect_equal(nrow(result), 2L)
  expect_true(all(c("id", "x", "y") %in% names(result)))
})

test_that("join_summaries handles multiple summaries with groups", {
  summary1 <- data.frame(id = c("a", "b"), x = c(1, 2))
  summary2 <- data.frame(id = c("a", "b"), y = c(3, 4))
  summary3 <- data.frame(id = c("a", "b"), z = c(5, 6))
  summaries <- list(first = summary1, second = summary2, third = summary3)

  result <- join_summaries(summaries, "id")

  expect_equal(nrow(result), 2L)
  expect_true(all(c("id", "x", "y", "z") %in% names(result)))
})

test_that("join_summaries preserves row order", {
  summary1 <- data.frame(id = c("a", "b"), x = c(1, 2))
  summary2 <- data.frame(id = c("b", "a"), y = c(4, 3))
  summaries <- list(first = summary1, second = summary2)

  result <- join_summaries(summaries, "id")

  expect_equal(result$id, c("a", "b"))
  expect_equal(result$y, c(3, 4))
})

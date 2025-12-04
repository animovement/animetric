# tests/testthat/test-new-aniframe-kin.R

# ------------------------------------------------------------------
# What we are testing
# ------------------------------------------------------------------
# 1  `new_aniframe_kin()`   creates an object of class "aniframe_kin"
# 2  `new_aniframe_kin2d()` creates an object of class "aniframe_kin2d"
# 3  `new_aniframe_kin3d()` creates an object of class "aniframe_kin3d"
# 4  All three constructors preserve the original data‑frame columns
# 5  The resulting objects inherit from `"data.frame"` (or `"tbl_df"` etc.)
# 6  The class vector contains each custom class exactly once
# 7  Objects created from a minimal aniframe (time, x, y) pass `aniframe::as_aniframe()`
# ------------------------------------------------------------------

# Helper: a minimal, valid aniframe‑like data frame
make_minimal_aniframe <- function() {
  data.frame(
    time = seq.POSIXt(from = Sys.time(), by = "sec", length.out = 5),
    x = runif(5),
    y = runif(5)
  )
}

test_that("new_aniframe_kin creates a proper aniframe_kin object", {
  df <- make_minimal_aniframe()
  kin <- new_aniframe_kin(df)

  # class checks
  expect_true(inherits(kin, "aniframe_kin"))
  expect_true(inherits(kin, "data.frame"))
  expect_equal(length(which(kin %>% class() == "aniframe_kin")), 1)

  # column preservation
  expect_named(kin, names(df))

  # conversion back to aniframe works
  expect_s3_class(aniframe::as_aniframe(kin), "aniframe")
})

test_that("new_aniframe_kin2d creates a proper aniframe_kin2d object", {
  df <- make_minimal_aniframe()
  kin2d <- new_aniframe_kin2d(df)

  expect_true(inherits(kin2d, "aniframe_kin2d"))
  expect_true(inherits(kin2d, "data.frame"))
  expect_equal(length(which(kin2d %>% class() == "aniframe_kin2d")), 1)

  expect_named(kin2d, names(df))
  expect_s3_class(aniframe::as_aniframe(kin2d), "aniframe")
})

test_that("new_aniframe_kin3d creates a proper aniframe_kin3d object", {
  df <- make_minimal_aniframe()
  kin3d <- new_aniframe_kin3d(df)

  expect_true(inherits(kin3d, "aniframe_kin3d"))
  expect_true(inherits(kin3d, "data.frame"))
  expect_equal(length(which(kin3d %>% class() == "aniframe_kin3d")), 1)

  expect_named(kin3d, names(df))
  expect_s3_class(aniframe::as_aniframe(kin3d), "aniframe")
})

test_that("custom classes appear only once in the class vector", {
  df <- make_minimal_aniframe()

  kin <- new_aniframe_kin(df)
  kin2d <- new_aniframe_kin2d(df)
  kin3d <- new_aniframe_kin3d(df)

  # The helper `unique()` inside the constructors should guarantee no duplicates
  expect_identical(class(kin), unique(class(kin)))
  expect_identical(class(kin2d), unique(class(kin2d)))
  expect_identical(class(kin3d), unique(class(kin3d)))
})

test_that("error when not an aniframe_kin", {
  df <- aniframe::example_aniframe()

  # Should give an error when it is not an aniframe_kin class
  expect_error(ensure_is_aniframe_kin(df))
})

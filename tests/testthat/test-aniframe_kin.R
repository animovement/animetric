# tests/testthat/test-new-aniframe-kin.R

# ------------------------------------------------------------------
# What we are testing
# ------------------------------------------------------------------
# 1  `new_aniframe_kin()`   creates an object of class "aniframe_kin"
# 2  It preserves the original data‑frame columns
# 3  The resulting object inherits from `"data.frame"` (or `"tbl_df"` etc.)
# 4  The class vector contains the class exactly once
# 5  Objects created from a minimal aniframe (time, x, y) pass `aniframe::as_aniframe()`
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

test_that("the class appears only once in the class vector", {
  df <- make_minimal_aniframe()

  # The `unique()` inside the constructor should guarantee no duplicates,
  # including when it is applied to an object that already carries it.
  kin <- new_aniframe_kin(new_aniframe_kin(df))

  expect_identical(class(kin), unique(class(kin)))
  expect_equal(sum(class(kin) == "aniframe_kin"), 1L)
})

test_that("error when not an aniframe_kin", {
  df <- aniframe::example_aniframe()

  # Should give an error when it is not an aniframe_kin class
  expect_error(ensure_is_aniframe_kin(df))
})

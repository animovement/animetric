# test-compute-tortuosity.R
# Tests for tortuosity compute functions
#
# Outline:
# --------
# compute_straightness:
#   - Returns correct ratio D/L for basic inputs
#   - Returns 1 for perfectly straight path (D = L)
#   - Returns 0 when displacement is 0
#   - Returns NA when path_length is 0
#   - Returns NA when path_length is NA
#   - Returns NA when displacement is NA
#   - Works with vector inputs
#   - Result is bounded [0, 1] for valid inputs
#
# compute_sinuosity:
#   - Returns correct value with "corrected" method (default)
#   - Returns correct value with "original" method
#   - Returns NA when mean_cos_turning >= 1
#   - Returns NA when mean_cos_turning is NA
#   - Returns NA when mean_step_length <= 0
#   - Returns NA when mean_step_length is NA
#   - Works with vector inputs
#   - Higher sinuosity for more tortuous paths (lower mean_cos)
#
# compute_emax:
#   - Returns correct dimensionless value (E_max-a)
#   - Returns correct dimensional value when dimensional = TRUE
#   - Returns dimensionless value when dimensional = TRUE but mean_step_length = NULL
#   - Returns NA when mean_cos_turning >= 1
#   - Returns NA when mean_cos_turning is NA
#   - Works with vector inputs
#   - Higher emax for straighter paths (higher mean_cos)

# =============================================================================
# compute_straightness
# =============================================================================

test_that("compute_straightness returns correct ratio", {
  expect_equal(compute_straightness(50, 100), 0.5)
  expect_equal(compute_straightness(10, 40), 0.25)
  expect_equal(compute_straightness(100, 100), 1.0)
})

test_that("compute_straightness returns 1 for perfectly straight path", {
  expect_equal(compute_straightness(100, 100), 1.0)
})

test_that("compute_straightness returns 0 when displacement is 0", {
  expect_equal(compute_straightness(0, 100), 0)
})

test_that("compute_straightness returns NA when path_length is 0", {
  expect_true(is.na(compute_straightness(0, 0)))
  expect_true(is.na(compute_straightness(50, 0)))
})

test_that("compute_straightness returns NA when path_length is NA", {
  expect_true(is.na(compute_straightness(50, NA)))
})

test_that("compute_straightness returns NA when displacement is NA", {
  expect_true(is.na(compute_straightness(NA, 100)))
})

test_that("compute_straightness works with vector inputs", {
  displacement <- c(50, 100, 0, 25)
  path_length <- c(100, 100, 100, 100)
  result <- compute_straightness(displacement, path_length)

  expect_length(result, 4)
  expect_equal(result, c(0.5, 1.0, 0.0, 0.25))
})

test_that("compute_straightness handles mixed valid/invalid in vectors", {
  displacement <- c(50, 100, NA, 25)
  path_length <- c(100, 0, 100, NA)
  result <- compute_straightness(displacement, path_length)

  expect_equal(result[1], 0.5)
  expect_true(is.na(result[2])) # path_length = 0
  expect_true(is.na(result[3])) # displacement = NA
  expect_true(is.na(result[4])) # path_length = NA
})

test_that("compute_straightness is bounded [0, 1] for valid inputs", {
  # Displacement can't exceed path length in real data
  result <- compute_straightness(c(0, 25, 50, 75, 100), rep(100, 5))
  expect_true(all(result >= 0 & result <= 1))
})

# =============================================================================
# compute_sinuosity
# =============================================================================
test_that("compute_sinuosity returns correct value with corrected method", {
  # Benhamou 2004, Equation 8: S = 2 / sqrt(p * (1+c)/(1-c))
  mean_step <- 10
  mean_cos <- 0.5
  expected <- 2 / sqrt(mean_step * (1 + mean_cos) / (1 - mean_cos))

  result <- compute_sinuosity(mean_step, mean_cos, method = "corrected")
  expect_equal(result, expected)
})

test_that("compute_sinuosity returns correct value with original method", {
  # Bovet & Benhamou 1988: S = 2 / sqrt(p * (1-c))
  mean_step <- 10
  mean_cos <- 0.5
  expected <- 2 / sqrt(mean_step * (1 - mean_cos))

  result <- compute_sinuosity(mean_step, mean_cos, method = "original")
  expect_equal(result, expected)
})

test_that("compute_sinuosity uses corrected method by default", {
  result_default <- compute_sinuosity(10, 0.5)
  result_corrected <- compute_sinuosity(10, 0.5, method = "corrected")
  expect_equal(result_default, result_corrected)
})

test_that("compute_sinuosity returns NA when mean_cos_turning > 1", {
  expect_true(is.na(compute_sinuosity(10, 1.5)))
})

test_that("compute_sinuosity returns NA when mean_cos_turning is NA", {
  expect_true(is.na(compute_sinuosity(10, NA)))
})

test_that("compute_sinuosity returns NA when mean_step_length <= 0", {
  expect_true(is.na(compute_sinuosity(0, 0.5)))
  expect_true(is.na(compute_sinuosity(-5, 0.5)))
})

test_that("compute_sinuosity returns NA when mean_step_length is NA", {
  expect_true(is.na(compute_sinuosity(NA, 0.5)))
})

test_that("compute_sinuosity works with vector inputs", {
  mean_step <- c(10, 10, 10)
  mean_cos <- c(0.2, 0.5, 0.8)
  result <- compute_sinuosity(mean_step, mean_cos)

  expect_length(result, 3)
  expect_true(all(!is.na(result)))
})

test_that("compute_sinuosity is higher for more tortuous paths", {
  # Lower mean_cos (more turning) = higher sinuosity
  mean_step <- 10
  sinuosity_tortuous <- compute_sinuosity(mean_step, 0.2)
  sinuosity_straight <- compute_sinuosity(mean_step, 0.8)

  expect_gt(sinuosity_tortuous, sinuosity_straight)
})

# =============================================================================
# compute_emax
# =============================================================================

test_that("compute_emax returns correct dimensionless value", {
  # E_max-a = sqrt((1+c)/(1-c))
  mean_cos <- 0.5
  expected <- sqrt((1 + mean_cos) / (1 - mean_cos))

  result <- compute_emax(mean_cos)
  expect_equal(result, expected)
})

test_that("compute_emax returns correct dimensional value", {
  # E_max-b = E_max-a * mean_step_length
  mean_cos <- 0.5
  mean_step <- 10
  emax_a <- sqrt((1 + mean_cos) / (1 - mean_cos))
  expected <- emax_a * mean_step

  result <- compute_emax(mean_cos, mean_step, dimensional = TRUE)
  expect_equal(result, expected)
})

test_that("compute_emax returns dimensionless when dimensional = TRUE but mean_step_length = NULL", {
  mean_cos <- 0.5
  result_null <- compute_emax(
    mean_cos,
    mean_step_length = NULL,
    dimensional = TRUE
  ) |>
    suppressWarnings()
  result_dimensionless <- compute_emax(mean_cos, dimensional = FALSE)

  expect_equal(result_null, result_dimensionless)
})

test_that("compute_emax gives warning when dimensional = TRUE but mean_step_length = NULL", {
  mean_cos <- 0.5
  expect_warning(compute_emax(
    mean_cos,
    mean_step_length = NULL,
    dimensional = TRUE
  ))
})

test_that("compute_emax returns NA when mean_cos_turning > 1", {
  expect_true(is.na(compute_emax(1.5)))
})

test_that("compute_emax returns Inf for a straight path", {
  expect_true(is.infinite(compute_emax(1.0)))
})

test_that("compute_emax returns NA when mean_cos_turning is NA", {
  expect_true(is.na(compute_emax(NA)))
})

test_that("compute_emax works with vector inputs", {
  mean_cos <- c(0.2, 0.5, 0.8)
  result <- compute_emax(mean_cos)

  expect_length(result, 3)
  expect_true(all(!is.na(result)))
})

test_that("compute_emax is higher for straighter paths", {
  # Higher mean_cos (less turning) = higher emax
  emax_tortuous <- compute_emax(0.2)
  emax_straight <- compute_emax(0.8)

  expect_gt(emax_straight, emax_tortuous)
})

test_that("compute_emax handles mixed valid/invalid in vectors", {
  mean_cos <- c(0.5, 1.0, NA, 0.8)
  result <- compute_emax(mean_cos)

  expect_false(is.na(result[1]))
  expect_true(is.infinite(result[2])) # mean_cos = 1
  expect_true(is.na(result[3])) # NA
  expect_false(is.na(result[4]))
})

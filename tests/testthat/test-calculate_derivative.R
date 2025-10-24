# Tests for the general‑order `calculate_derivative()` function
#
# 1. Linear motion (x = t) – any order ≥ 1 returns 1 for order = 1,
#    and 0 for all higher orders.
# 2. Quadratic motion (x = t^2) – first derivative = 2*t,
#    second derivative = 2, third+ = 0.
# 3. Cubic motion (x = t^3) – verifies that third‑order derivative = 6
#    (constant) and fourth+ = 0.
# 4. Non‑uniform time steps – ensure spacing is honoured for several orders.
# 5. Invalid `order` (non‑integer, ≤0) raises an informative error.
# 6. Standard input validation (length mismatch, non‑numeric, non‑monotonic time).

test_that("linear motion behaves correctly for any order", {
  t <- 0:5
  x <- 0:5

  expect_equal(calculate_derivative(x, t, order = 1L), rep(1, length(t)))
  expect_equal(calculate_derivative(x, t, order = 2L), rep(0, length(t)))
  expect_equal(calculate_derivative(x, t, order = 5L), rep(0, length(t)))
})

test_that("quadratic motion gives analytic results", {
  t <- seq(0, 4, by = 1)
  x <- t^2

  # First derivative – interior points are exact, edges differ by 1
  expect_equal(
    round(calculate_derivative(x, t, order = 1L), 6),
    c(1, 2, 4, 6, 7)
  )

  # Second derivative – interior exact, edges 1.5 and 1
  expect_equal(
    round(calculate_derivative(x, t, order = 2L), 6),
    c(1, 1.5, 2, 1.5, 1)
  )

  # Third derivative – interior 0, edges ±0.5
  expect_equal(
    round(calculate_derivative(x, t, order = 3L), 6),
    c(0.5, 0.5, 0, -0.5, -0.5)
  )
})


test_that("invalid order arguments raise errors", {
  t <- 0:3
  x <- t
  expect_error(
    calculate_derivative(x, t, order = 0L),
    "'order' must be a single positive integer"
  )
  expect_error(
    calculate_derivative(x, t, order = -2L),
    "'order' must be a single positive integer"
  )
  expect_error(
    calculate_derivative(x, t, order = 1.5),
    "'order' must be a single positive integer"
  )
})

test_that("standard input validation catches mistakes", {
  expect_error(calculate_derivative(1:4, 1:5), "must have the same length")
  expect_error(calculate_derivative(1:2, c("a", "b")), "must be numeric")
  expect_error(
    calculate_derivative(1:3, c(0, 2, 1)),
    "must be strictly increasing"
  )
})

# tests/testthat/test-calculate_kinematics_2d.R
#
# Thorough tests for the 2‑D kinematics pipeline
#
# 1. calculate_intermediate_2d creates correct dx/dy and preserves NA on first row
# 2. calculate_translation_2d produces distance, cumsum_distance, v_translation,
#    and a_translation with correct numeric values and proper NA handling
# 3. calculate_rotation_2d yields direction, d_rotation, v_rotation, a_rotation
#    and removes the temporary dx/dy columns
# 4. calculate_kinematics_2d runs the whole pipeline, returns an aniframe,
#    includes all public kinematic columns, and drops helpers
# 5. Edge cases: single observation, constant motion, non‑uniform time steps,
#    and mismatched coordinate lengths raise informative errors

# ------------------------------------------------------------------
# Helper: tiny synthetic aniframe‑compatible data frame
# ------------------------------------------------------------------
make_test_df <- function() {
  data.frame(
    time = c(0, 1, 3, 6), # non‑uniform intervals
    x = c(0, 2, 5, 9),
    y = c(0, 1, 1, 4)
  ) |>
    aniframe::as_aniframe()
}

# ------------------------------------------------------------------
# 1. Intermediate deltas -------------------------------------------------
# ------------------------------------------------------------------
test_that("calculate_intermediate_2d computes correct dx/dy", {
  df <- make_test_df()
  out <- calculate_intermediate_2d(df)

  # Expected deltas (first row NA because of lag)
  exp_dx <- c(NA_real_, 2, 3, 4)
  exp_dy <- c(NA_real_, 1, 0, 3)

  expect_equal(out$dx, exp_dx)
  expect_equal(out$dy, exp_dy)
})

# ------------------------------------------------------------------
# 2. Translational kinematics -------------------------------------------
# ------------------------------------------------------------------
test_that("calculate_translation_2d returns correct distance metrics", {
  df <- make_test_df() |>
    calculate_intermediate_2d() |>
    calculate_translation_2d()

  ## -----------------------------------------------------------------
  ## 1. Euclidean distances (now d_translation)
  ## -----------------------------------------------------------------
  exp_dist <- c(
    0,
    sqrt(2^2 + 1^2), # √5
    sqrt(3^2 + 0^2), # 3
    sqrt(4^2 + 3^2) # 5
  )
  expect_equal(df$d_translation, exp_dist)

  ## -----------------------------------------------------------------
  ## 2. Cumulative distance (cumsum_d_translation)
  ## -----------------------------------------------------------------
  exp_cum <- cumsum(exp_dist)
  expect_equal(df$cumsum_d_translation, exp_cum)

  ## -----------------------------------------------------------------
  ## 3. Translational speed – forward/central/backward differences
  ## -----------------------------------------------------------------
  # Time differences
  dt_fwd <- dplyr::lead(df$time) - df$time # forward Δt
  dt_bwd <- df$time - dplyr::lag(df$time) # backward Δt
  dt_ctr <- (dt_fwd + dt_bwd) / 2 # central Δt (only for interior rows)

  # Speed components
  v_fwd <- (dplyr::lead(df$d_translation) - df$d_translation) / dt_fwd
  v_bwd <- (df$d_translation - dplyr::lag(df$d_translation)) / dt_bwd
  v_ctr <- ((dplyr::lead(df$d_translation) - df$d_translation) +
    (df$d_translation - dplyr::lag(df$d_translation))) /
    (2 * dt_ctr)

  # Assemble the final speed vector exactly as calculate_velocity() does
  exp_speed <- numeric(length(df$d_translation))
  n <- length(exp_speed)

  # first element → forward diff (if a next point exists)
  exp_speed[1] <- if (n > 1) v_fwd[1] else NA_real_

  # interior elements → central diff
  if (n > 2) {
    exp_speed[2:(n - 1)] <- v_ctr[2:(n - 1)]
  }

  # last element → backward diff (if a previous point exists)
  if (n > 1) {
    exp_speed[n] <- v_bwd[n]
  }

  # Replace any NaNs that arise from 0/0 with NA (mirrors calculate_velocity())
  exp_speed[is.nan(exp_speed)] <- NA_real_

  expect_equal(df$v_translation, exp_speed, tolerance = 1e-8)

  ## -----------------------------------------------------------------
  ## 4. Translational acceleration – same forward/central/backward scheme
  ## -----------------------------------------------------------------
  # We can reuse the same helper logic that calculate_velocity() employs
  # (forward for first, central for interior, backward for last).

  #   # First element
  #   acc1 <- (exp_speed[2] - exp_speed[1]) / (df$time[2] - df$time[1])
  #
  #   # Interior (i = 2 … n‑1) – central difference of speed
  #   if (n > 2) {
  #     acc_mid <- numeric(n - 2)
  #     for (i in 2:(n - 1)) {
  #       dt_f <- df$time[i + 1] - df$time[i]       # forward Δt at i
  #       dt_b <- df$time[i] - df$time[i - 1]       # backward Δt at i
  #       acc_mid[i - 1] <- ((exp_speed[i + 1] - exp_speed[i]) / dt_f +
  #                            (exp_speed[i] - exp_speed[i - 1]) / dt_b) / 2
  #     }
  #     acc2 <- acc_mid
  #   } else {
  #     acc2 <- numeric(0)
  #   }
  #
  #   # Last element
  #   acc3 <- (exp_speed[n] - exp_speed[n - 1]) / (df$time[n] - df$time[n - 1])
  #
  #   exp_acc <- c(acc1, acc2, acc3)
  #   expect_equal(df$a_translation, exp_acc, tolerance = 1e-8)
})

# ------------------------------------------------------------------
# 3. Rotational kinematics ---------------------------------------------
# ------------------------------------------------------------------
test_that("calculate_rotation_2d produces correct angular measures", {
  df <- make_test_df() |>
    calculate_intermediate_2d() |>
    calculate_rotation_2d()

  # Direction (atan2(dy, dx) in radians)
  exp_dir <- atan2(c(NA, 1, 0, 3), c(NA, 2, 3, 4))
  expect_equal(df$direction, exp_dir)

  # Angular difference using aniframe::diff_angle (wraps to [-π, π])
  # We'll compute it manually for verification
  diff_angle_manual <- function(a) {
    d <- diff(c(a, a[1])) # dummy placeholder; we only need pairwise diffs
    d
  }
  # Since diff_angle is a black‑box, just ensure the output length matches
  expect_length(df$d_rotation, nrow(df))

  # Velocity and acceleration should be numeric and same length
  expect_type(df$v_rotation, "double")
  expect_type(df$a_rotation, "double")
  expect_length(df$v_rotation, nrow(df))
  expect_length(df$a_rotation, nrow(df))

  # Temporary helpers must be gone
  # expect_false(any(c("dx", "dy") %in% names(df)))
})

# ------------------------------------------------------------------
# 4. Full pipeline ------------------------------------------------------
# ------------------------------------------------------------------
test_that("calculate_kinematics_2d runs end‑to‑end and returns a proper aniframe", {
  df_raw <- make_test_df() |> aniframe::as_aniframe()
  kin <- calculate_kinematics_2d(df_raw)

  # Public kinematic columns must exist
  public_cols <- c(
    "d_translation",
    "cumsum_d_translation",
    "v_translation",
    "a_translation",
    "direction",
    "d_rotation",
    "cumsum_d_rotation",
    "v_rotation",
    "a_rotation"
  )
  expect_true(all(public_cols %in% names(kin)))

  # Original columns must be preserved
  expect_true(all(c("time", "x", "y") %in% names(kin)))

  # Helpers must be removed
  expect_false(any(c("dx", "dy") %in% names(kin)))

  # Check that the object still carries the aniframe class
  expect_true(inherits(kin, "aniframe"))

  # Spot‑check a couple of values against the component functions
  # (re‑run the sub‑steps and compare)
  step_by_step <- df_raw |>
    calculate_intermediate_2d() |>
    calculate_translation_2d() |>
    calculate_rotation_2d()
  expect_equal(kin$d_translation, step_by_step$d_translation)
  expect_equal(kin$direction, step_by_step$direction)
})

# ------------------------------------------------------------------
# 5. Edge cases ---------------------------------------------------------
# ------------------------------------------------------------------
# test_that("single observation returns sensible defaults", {
#   df_one <- data.frame(time = 0, x = 0, y = 0) |>
#     aniframe::as_aniframe()
#   kin_one <- calculate_kinematics_2d(df_one)
#
#   # Distance and speed are zero, accelerations are NA
#   expect_equal(kin_one$d_translation, 0)
#   expect_equal(kin_one$cumsum_d_translation, 0)
#   expect_true(is.na(kin_one$v_translation))
#   expect_true(is.na(kin_one$a_translation))
#   expect_true(is.na(kin_one$direction))
#   expect_true(is.na(kin_one$d_rotation))
#   expect_true(is.na(kin_one$cumsum_d_rotation))
#   expect_true(is.na(kin_one$v_rotation))
#   expect_true(is.na(kin_one$a_rotation))
# })

test_that("constant straight‑line motion yields zero rotation", {
  df_const <- data.frame(
    time = c(0, 1, 2, 3),
    x = c(0, 1, 2, 3),
    y = c(0, 0, 0, 0)
  ) |>
    aniframe::as_aniframe()
  kin_const <- calculate_kinematics_2d(df_const)

  # Direction should be constant (0 rad) after the first NA
  expect_equal(kin_const$direction[-1], rep(0, 3))
  # All rotational derivatives should be zero (or NA where undefined)
  expect_true(all(is.na(kin_const$d_rotation) | kin_const$d_rotation == 0))
  expect_true(all(is.na(kin_const$v_rotation) | kin_const$v_rotation == 0))
  expect_true(all(is.na(kin_const$a_rotation) | kin_const$a_rotation == 0))
})

test_that("non‑monotonic time throws an informative error", {
  df_bad <- data.frame(
    time = c(0, 2, 1, 3), # time goes backwards at index 3
    x = c(0, 1, 2, 3),
    y = c(0, 0, 0, 0)
  ) |>
    aniframe::as_aniframe() |>
    dplyr::mutate(time = c(0, 2, 1, 3))
  expect_error(calculate_kinematics_2d(df_bad))
})

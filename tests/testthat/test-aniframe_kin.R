# The aniframe_kin class is retired (#58): add_kinematics() no longer
# adds it, and is_aniframe_kin() is deprecated.

test_that("add_kinematics() returns a plain anipoint", {
  kin <- add_kinematics(
    anicore::example_anipoint(n_obs = 5, n_individuals = 1, n_keypoints = 1)
  )

  expect_s3_class(kin, "anipoint")
  expect_false(inherits(kin, "aniframe_kin"))
})

test_that("is_aniframe_kin() is deprecated and checks for kinematic columns", {
  af <- anicore::example_anipoint(n_obs = 5, n_individuals = 1, n_keypoints = 1)
  kin <- add_kinematics(af)

  expect_warning(
    expect_true(is_aniframe_kin(kin)),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    expect_false(is_aniframe_kin(af)),
    class = "lifecycle_warning_deprecated"
  )
  expect_warning(
    expect_false(is_aniframe_kin(data.frame(speed = 1))),
    class = "lifecycle_warning_deprecated"
  )
})

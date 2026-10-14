# The aniframe_kin class is retired (#58): add_kinematics() no longer
# adds it.

test_that("add_kinematics() returns a plain anipoint", {
  kin <- add_kinematics(
    anicore::example_anipoint(n_obs = 5, n_individuals = 1, n_keypoints = 1)
  )

  expect_s3_class(kin, "anipoint")
  expect_false(inherits(kin, "aniframe_kin"))
})

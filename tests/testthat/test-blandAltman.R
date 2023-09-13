test_that("blandAltman works as expected with default settings", {
  data("platelet")
  res <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  expect_s4_class(res, "BAsummary")
  expect_equal(res@param, list(
    type1 = 3,
    type2 = 5,
    conf.level = 0.95
  ))
  object <- matrix(
    c(
      120, 7.33000000, 6.35000000, -47.8000000, 42.1000000, 0.150000000, 15.7500000,
      15.990447, 1.45972140, -24.0106997, 38.6706997, 4.46899864, 10.19100136,
      120, 0.06356753, 0.05463985, -0.4117647, 0.6666667, 0.001067738, 0.1180876,
      0.144754, 0.01321417, -0.2201451, 0.3472802, 0.03766822, 0.08946683
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(
      c("absolute_difference", "relative_difference"),
      c(
        "n", "mean", "median", "min", "max", "q1", "q3", "sd", "se",
        "limit_lr", "limit_ur", "ci_lr", "ci_ur"
      )
    )
  )
  expect_equal(res@stat$tab, object)
})

test_that("blandAltman works as expected with specific types", {
  data("platelet")
  res <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, type1 = 1, type2 = 4)
  expect_equal(res@param, list(
    type1 = 1,
    type2 = 4,
    conf.level = 0.95
  ))
  object <- matrix(
    c(
      120, 7.33000000, 6.35000000, -47.8000000, 42.1, 0.150000000, 15.7500000,
      15.9904467, 1.45972140, -24.0106997, 38.670700, 4.46899864, 10.1910014,
      120, 0.07824269, 0.05617518, -0.3414634, 1.0, 0.001080224, 0.1254982,
      0.1730707, 0.01579912, -0.2609697, 0.417455, 0.04727698, 0.1092084
    ),
    nrow = 2, byrow = TRUE,
    dimnames = list(
      c("absolute_difference", "relative_difference"),
      c(
        "n", "mean", "median", "min", "max", "q1", "q3", "sd", "se",
        "limit_lr", "limit_ur", "ci_lr", "ci_ur"
      )
    )
  )
  expect_equal(res@stat$tab, object)
})

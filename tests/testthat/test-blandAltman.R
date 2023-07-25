test_that("blandAltman works as expected with default settings", {
  data("platelet")
  res <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  expect_s4_class(res, "BAsummary")
  expect_identical(dim(res@stat$tab), c(2L, 13L))
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

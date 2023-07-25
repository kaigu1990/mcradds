# show ----

test_that("show works as expected for SampleSize object", {
  object <- size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson")
  result <- capture_output(show(object))
  expect_match(result, "Given Lower Confidence Interval", fixed = TRUE)
  expect_match(result, "optimal sample size: n = 246", fixed = TRUE)
  expect_match(result, "p:0.85 lr:0.8 alpha:0.05 interval:c(1, 1e+05)", fixed = TRUE)
  expect_match(result, "tol:1e-05 alternative:two.sided method:wilson", fixed = TRUE)
})

test_that("show works as expected for BAsummary object", {
  data("platelet")
  object <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  result <- capture_output(show(object))
  expect_match(result, "Absolute.difference Relative.difference", fixed = TRUE)
  expect_match(result, "Mean (SD)                        7.330 (15.990)", fixed = TRUE)
  expect_match(result, "Limit of Agreement            (-24.011, 38.671)", fixed = TRUE)
  expect_match(result, "Confidence Interval of Mean    ( 4.469, 10.191)", fixed = TRUE)
})

# getOutlier ----

test_that("getOutlier works as expected with default settings", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  object <- getOutlier(ba, method = "ESD", difference = "rel")
  expect_identical(object$stat$Outlier, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(object$sid, c(1, 4, 2, 10))
})

test_that("getOutlier works as expected with sample id", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, sid = platelet$Sample)
  object <- getOutlier(ba, method = "ESD", difference = "rel")
  expect_identical(object$sid, c("ID1", "ID4", "ID2", "ID10"))
})

test_that("getOutlier works as expected with 4E method", {
  data("creatinine", package = "mcr")
  ba <- blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea)
  object <- getOutlier(ba, method = "4E")
  expect_equal(dim(object$stat), c(6, 8))
  expect_identical(object$ord, c(4L, 51L, 96L, 97L, 106L, 108L))
})

test_that("getOutlier works as expected to print no outlier", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, sid = platelet$Sample)
  result <- capture_output(getOutlier(ba, method = "4E"))
  expect_character(result, pattern = "No outlier is detected.")
})

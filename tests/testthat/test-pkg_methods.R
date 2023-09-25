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
  expect_match(result, "Absolute difference type:  Y-X", fixed = TRUE)
  expect_match(result, "Relative difference type:  (Y-X)/(0.5*(X+Y))", fixed = TRUE)
  expect_match(result, "Absolute.difference Relative.difference", fixed = TRUE)
  expect_match(result, "Mean (SD)                        7.330 (15.990)", fixed = TRUE)
  expect_match(result, "Limit of Agreement            (-24.011, 38.671)", fixed = TRUE)
  expect_match(result, "Confidence Interval of Mean    ( 4.469, 10.191)", fixed = TRUE)
})

test_that("show works as expected for RefInt object", {
  data("calcium")
  object <- refInterval(calcium$Value, RI_method = "nonparametric", CI_method = "nonparametric")
  result <- capture_output(show(object))
  expect_match(result, "Outliers: NULL", fixed = TRUE)
  expect_match(result, "Reference Interval: 9.10, 10.30", fixed = TRUE)
  expect_match(result, "RefLower Confidence Interval: 8.9000, 9.2000", fixed = TRUE)
  expect_match(result, "Refupper Confidence Interval: 10.3000, 10.4000", fixed = TRUE)
})

test_that("show works as expected for tpROC object", {
  data("ldlroc")
  object <- expect_silent(aucTest(
    x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
    levels = c(0, 1), direction = "<"
  ))
  result <- capture_output(show(object))
  expect_match(result, "The hypothesis for testing difference based on Paired ROC curve", fixed = TRUE)
  expect_match(result, "Test assay:\n  Area under the curve: 0.7995", fixed = TRUE)
  expect_match(result, "Reference/standard assay:\n  Area under the curve: 0.5617", fixed = TRUE)
  expect_match(result, "Alternative hypothesis: the difference in AUC is difference to 0", fixed = TRUE)
  expect_match(result, "Difference of AUC: 0.2378", fixed = TRUE)
  expect_match(result, "Standard Error(SE): 0.0790\n  95% Confidence Interval(CI): 0.0829-0.3927", fixed = TRUE)
  expect_match(result, "Z: 3.0088\n  Pvalue: 0.002623", fixed = TRUE)
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
  ba <- blandAltman(x = c(1:10), y = c(2:8, 50, 20, 30))
  object <- getOutlier(ba, method = "4E")
  expect_equal(dim(object$stat), c(1, 8))
  expect_identical(object$ord, c(8L))
})

test_that("getOutlier works as expected to print no outlier", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, sid = platelet$Sample)
  result <- capture_output(getOutlier(ba, method = "4E"))
  expect_character(result, pattern = "No outlier is detected.")
})

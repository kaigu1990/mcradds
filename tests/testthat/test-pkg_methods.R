# show ----

test_that("show works as expected for SampleSize object", {
  object <- size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson")
  result <- capture_output(show(object))
  expect_match(result, "Given Lower Confidence Interval", fixed = TRUE)
  expect_match(result, "optimal sample size: n = 246", fixed = TRUE)
  expect_match(result, "p:0.85 lr:0.8 alpha:0.05 interval:c(1, 1e+05)", fixed = TRUE)
  expect_match(result, "tol:1e-05 alternative:two.sided method:wilson", fixed = TRUE)
})

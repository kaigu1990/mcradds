# size_one_prop ----

test_that("size_one_prop function works as expected with default settings", {
  object <- size_one_prop(p1 = 0.95, p0 = 0.9)
  expect_s4_class(object, "SampleSize")
  expect_equal(object@n, 238.0332, tolerance = 0.001)
})

# size_ci_one_prop ----

test_that("size_ci_one_prop function works as expected with custom settings", {
  object <- size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson")
  expect_s4_class(object, "SampleSize")
  expect_equal(object@n, 245.8534, tolerance = 0.00001)
})

test_that("size_ci_one_prop function works as expected with invalid settings", {
  expect_error(size_ci_one_prop(p = 0.85, lr = 0.9, alpha = 0.05, method = "wilson"))
  expect_error(size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson2"))
})

# size_corr ----

test_that("size_corr function works as expected with custom settings", {
  object <- size_corr(r1 = 0.95, r0 = 0.9, alpha = 0.025, power = 0.8, alternative = "greater")
  expect_s4_class(object, "SampleSize")
  expect_equal(object@n, 63.71021, tolerance = 0.00001)
})

test_that("size_corr function works as expected with invalid settings", {
  expect_error(size_corr(r1 = 0.9, r0 = 0.95))
})

# size_ci_corr ----

test_that("size_ci_corr function works as expected with custom settings", {
  object <- size_ci_corr(r = 0.9, lr = 0.85, alpha = 0.025, alternative = "greater")
  expect_s4_class(object, "SampleSize")
  expect_equal(object@n, 85.28498, tolerance = 0.001)
})

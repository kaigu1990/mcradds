# robustRI ----

test_that("robustRI works as expected", {
  x <- c(8.9, 9.2, rep(9.4, 2), rep(9.5, 3), rep(9.6, 4), rep(9.7, 5), 9.8, rep(9.9, 2), 10.2)
  res <- robustRI(x)
  expect_equal(res, c(9.049545, 10.199396))
})

# nonparRI ----

test_that("nonparRI works as expected", {
  data("calcium")
  res <- nonparRI(calcium$Value)
  expect_named(res, c("2.5%", "97.5%"))
  expect_equal(as.numeric(res), c(9.1, 10.3))
})

# refInterval ----

test_that("refInterval works as expected with default settings", {
  data("calcium")
  res <- expect_silent(refInterval(calcium$Value))
  expect_identical(res@method, "Reference Interval Method: parametric, Confidence Interval Method: parametric")
  expect_null(res@outlier$ord)
  expect_equal(res@refInt, c(9.051286, 10.317047), tolerance = 0.0001)
  expect_equal(res@confInt, list(
    refLower = c(8.992613, 9.109959),
    refUpper = c(10.25837, 10.37572)
  ), tolerance = 0.0001)
})

test_that("refInterval works as expected with non-parametric method and ci", {
  data("calcium")
  res <- expect_silent(refInterval(calcium$Value, RI_method = "nonparametric", CI_method = "nonparametric"))
  expect_identical(res@method, "Reference Interval Method: nonparametric, Confidence Interval Method: nonparametric")
  expect_equal(res@refInt, c(9.1, 10.3))
  expect_equal(res@confInt, list(
    refLower = c(8.9, 9.2),
    refUpper = c(10.3, 10.4)
  ))
})

test_that("refInterval works as expected with robust method and boot ci", {
  data("calcium")
  res <- refInterval(calcium$Value, RI_method = "robust", CI_method = "boot", rng.seed = 12365)
  expect_identical(res@method, "Reference Interval Method: robust, Confidence Interval Method: boot")
  expect_equal(res@refInt, c(9.038892, 10.315612), tolerance = 0.0001)
  expect_equal(res@confInt, list(
    refLower = c(8.980622, 9.097574),
    refUpper = c(10.25782, 10.37414)
  ), tolerance = 0.0001)
})

test_that("refInterval works as expected when the sample size is below to 120", {
  data("calcium")
  x <- sample(calcium$Value, 80)
  res <- suppressWarnings(refInterval(x, RI_method = "nonparametric", CI_method = "nonparametric", rng.seed = 12365))
  expect_warning(refInterval(x, RI_method = "nonparametric", CI_method = "nonparametric", rng.seed = 12365))
  expect_identical(res@method, "Reference Interval Method: nonparametric, Confidence Interval Method: boot")
  expect_equal(res@refInt, c(9.2000, 10.2025))
  expect_equal(res@confInt, list(
    refLower = c(8.9000, 9.2975),
    refUpper = c(10.2, 10.3)
  ))
})

test_that("refInterval works as expected with parametric in RI and nonparametric in CI", {
  data("calcium")
  res <- suppressWarnings(refInterval(calcium$Value, RI_method = "parametric", CI_method = "nonparametric"))
  expect_warning(refInterval(calcium$Value, RI_method = "parametric", CI_method = "nonparametric"))
  expect_identical(res@method, "Reference Interval Method: parametric, Confidence Interval Method: parametric")
})

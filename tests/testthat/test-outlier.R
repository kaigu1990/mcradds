# esd.critical ----

test_that("esd.critical works as expected", {
  expect_equal(esd.critical(alpha = 0.05, N = 100, i = 1), 3.384083, tolerance = 0.00001)
})

# ESD_test ----

test_that("ESD_test works as expected with default settings", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  res <- expect_silent(ESD_test(ba@stat$relative_diff))

  object <- data.frame(
    ESDi = c(4.166372, 3.872621, 3.797226, 3.903086, 3.318236, 2.970250),
    Lambda = c(3.445148, 3.442394, 3.439611, 3.436800, 3.433961, 3.431092),
    Outlier = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  expect_equal(res$stat[, 6:8], object, tolerance = 0.0001)
  expect_identical(res$ord, c(1L, 4L, 2L, 10L))
})

test_that("ESD_test works as expected when h is 10", {
  data("platelet")
  ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
  expect_warning(ESD_test(ba@stat$relative_diff, h = 10), "No more than 5% of sample results")

  res <- suppressWarnings(ESD_test(ba@stat$relative_diff, h = 10))
  expect_identical(nrow(res$stat), 11L)
})

# tukey_outlier ----

test_that("tukey_outlier works as expected", {
  x <- c(13.6, 44.4, 45.9, 14.9, 41.9, 53.3, 44.7, 95.2, 44.1, 50.7, 45.2, 60.1, 89.1)
  res <- expect_silent(tukey_outlier(x))

  object <- list(
    ord = c(1, 4, 8, 13),
    out = c(13.6, 14.9, 95.2, 89.1),
    subset = c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  )
  expect_equal(res, object)
})

# dixon_outlier ----

test_that("dixon_outlier works as expected", {
  x <- c(13.6, 44.4, 45.9, 11.9, 41.9, 53.3, 44.7, 95.2, 44.1, 50.7, 45.2, 60.1, 89.1)
  res <- expect_silent(dixon_outlier(x))

  object <- list(
    ord = c(1, 4, 8, 13),
    out = c(13.6, 11.9, 95.2, 89.1),
    subset = c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  )
  expect_equal(res, object)
})

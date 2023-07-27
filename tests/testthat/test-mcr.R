# mcreg ----

test_that("mcreg works as expected with Deming regression", {
  data(platelet)
  fit <- mcreg(
    x = platelet$Comparative, y = platelet$Candidate,
    method.reg = "Deming", method.ci = "jackknife"
  )
  expect_class(fit, "MCResultJackknife")
  object <- matrix(
    c(
      4.335885, 1.568968372, 1.2289002, 7.442869,
      1.012951, 0.009308835, 0.9945175, 1.031386
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("Intercept", "Slope"),
      c("EST", "SE", "LCI", "UCI")
    )
  )
  expect_equal(fit@para, object, tolerance = 0.00001)
})


# getCoefficients ----

test_that("getCoefficients works as expected with default settings", {
  data(platelet)
  fit <- mcreg(
    x = platelet$Comparative, y = platelet$Candidate,
    method.reg = "Deming", method.ci = "jackknife"
  )
  object <- matrix(
    c(
      4.335885, 1.568968372, 1.2289002, 7.442869,
      1.012951, 0.009308835, 0.9945175, 1.031386
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("Intercept", "Slope"),
      c("EST", "SE", "LCI", "UCI")
    )
  )
  expect_equal(getCoefficients(fit), object, tolerance = 0.00001)
})


# calcBias ----

test_that("calcBias works as expected with default settings", {
  data(platelet)
  fit <- mcreg(
    x = platelet$Comparative, y = platelet$Candidate,
    method.reg = "Deming", method.ci = "jackknife"
  )
  object <- matrix(
    c(
      30, 4.724429, 1.378232, 1.995155, 7.453704,
      200, 6.926183, 1.288534, 4.374535, 9.477832
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      c("X1", "X2"),
      c("Level", "Bias", "SE", "LCI", "UCI")
    )
  )
  expect_equal(calcBias(fit, x.levels = c(30, 200)), object, tolerance = 0.00001)
})

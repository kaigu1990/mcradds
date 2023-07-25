# h_factor ----

test_that("h_factor works as expected", {
  df <- data.frame(a = c("aa", "a", "aa"))
  res <- h_factor(df, var = "a")
  expect_class(res, "factor")
  expect_equal(levels(res), c("a", "aa"))

  res2 <- h_factor(df, var = "a", levels = c("aa", "a"))
  expect_equal(levels(res2), c("aa", "a"))
})

# h_summarize ----

test_that("h_summarize works as expected", {
  res <- h_summarize(1:50)
  expect_identical(colnames(res), c(
    "n", "mean", "median", "min", "max", "q1", "q3", "sd",
    "se", "limit_lr", "limit_ur", "ci_lr", "ci_ur"
  ))
})

# h_difference ----

test_that("h_difference works as expected", {
  res <- h_difference(x = c(1.1, 1.2, 1.5), y = c(1.2, 1.3, 1.4), type = 5)
  object <- matrix(
    c(1.1, 1.2, 1.5, 1.2, 1.3, 1.4, 1.15, 1.25, 1.45, 0.08695652, 0.08000000, -0.06896552),
    nrow = 3,
    byrow = FALSE,
    dimnames = list(
      NULL,
      c("x", "y", "x_ba", "y_ba")
    )
  )
  expect_equal(res, object, tolerance = 0.00001)
})

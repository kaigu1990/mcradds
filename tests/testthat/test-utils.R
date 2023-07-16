test_that("h_factor works as expected", {
  df <- data.frame(a = c("aa", "a", "aa"))
  res <- h_factor(df, var = "a")
  expect_class(res, "factor")
  expect_equal(levels(res), c("a", "aa"))

  res2 <- h_factor(df, var = "a", levels = c("aa", "a"))
  expect_equal(levels(res2), c("aa", "a"))
})

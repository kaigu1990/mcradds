# pearsonTest ----

test_that("pearsonTest works as expected with default settings that same as cor.test", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  res <- pearsonTest(x, y)
  object <- cor.test(x, y)
  expect_named(res$stat, c("cor", "lowerci", "upperci", "Z", "pval"))
  expect_equal(res$stat["cor"], object$estimate)
  expect_equal(as.numeric(c(res$stat[c("lowerci", "upperci")])), as.numeric(object$conf.int))
})

test_that("pearsonTest works as expected when H0 is 0.5 and alternative is greater", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  res <- pearsonTest(x, y, h0 = 0.5, alternative = "greater")
  expect_equal(as.numeric(res$stat[c("Z", "pval")]), c(0.2448722, 0.4032777), tolerance = 0.0001)
})

# spearmanTest ----

test_that("spearmanTest works as expected with default settings", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  res <- spearmanTest(x, y, rng.seed = 12369)
  object <- cor.test(x, y, method = "spearman")
  expect_named(res$stat, c("cor", "lowerci", "upperci", "Z", "pval"))
  expect_equal(as.numeric(res$stat["cor"]), as.numeric(object$estimate))
  expect_equal(
    res$stat,
    c(
      cor = 0.6000000, lowerci = -0.2075472, upperci = 0.9816514,
      Z = 1.5630040, pval = 0.1180517
    ),
    tolerance = 0.00001
  )
})

test_that("spearmanTest works as expected when H0 is 0.5 and alternative is greater", {
  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
  y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
  res <- spearmanTest(x, y, h0 = 0.5, alternative = "greater")
  expect_equal(as.numeric(res$stat[c("Z", "pval")]), c(0.3243526, 0.3728355), tolerance = 0.00001)
})

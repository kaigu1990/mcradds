test_that("diagTab function works as expected with default settings", {
  data("qualData")
  res <- qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
  expect_s4_class(res, "MCTab")
  object <- matrix(
    c(54, 8, 16, 122),
    nrow = 2, byrow = FALSE,
    dimnames = list(
      CandidateN = c("0", "1"),
      ComparativeN = c("0", "1")
    )
  )
  expect_equal(res@tab, object)
  expect_equal(levels(res@candidate$data), c("0", "1"))
  expect_equal(levels(res@comparative$data), c("0", "1"))
})

test_that("diagTab function works as expected with custom settings", {
  data("qualData")
  res <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  object <- matrix(
    c(122, 16, 8, 54),
    nrow = 2, byrow = FALSE,
    dimnames = list(
      CandidateN = c("1", "0"),
      ComparativeN = c("1", "0")
    )
  )
  expect_equal(res@tab, object)
})

test_that("diagTab function works as expected with wilson method", {
  data("qualData")
  tb <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  res <- getAccuracy(tb, method = "wilson")
  object <- matrix(
    c(
      "0.8841", "0.8200", "0.9274",
      "0.8710", "0.7655", "0.9331",
      "0.9385", "0.8833", "0.9685",
      "0.7714", "0.6605", "0.8541",
      "6.8514", "3.5785", "13.1181",
      "0.1331", "0.0832", "0.2131"
    ),
    nrow = 6, byrow = TRUE,
    dimnames = list(
      c("sens", "spec", "ppv", "npv", "plr", "nlr"),
      ComparativeN = c("EST", "LowerCI", "UpperCI")
    )
  )
  expect_equal(res, as.data.frame(object))
})

test_that("diagTab function works as expected when withref is FALSE", {
  data("qualData")
  tb <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  res <- getAccuracy(tb, method = "wilson", withref = FALSE)
  object <- matrix(
    c(
      "0.8841", "0.8200", "0.9274",
      "0.8710", "0.7655", "0.9331",
      "0.8800", "0.8277", "0.9180"
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(
      c("ppa", "npa", "opa"),
      ComparativeN = c("EST", "LowerCI", "UpperCI")
    )
  )
  expect_equal(res, as.data.frame(object))
})

test_that("diagTab function works as expected with custom settings", {
  data("qualData")
  res <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  object <- matrix(
    c(122, 16, 8, 54),
    nrow = 2, byrow = FALSE,
    dimnames = list(
      CandidateN = c("1", "0"),
      ComparativeN = c("1", "0")
    )
  )
  expect_equal(res@tab, object)
})

test_that("getAccuracy function works as expected with digits is 3", {
  data("qualData")
  tb <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  res <- getAccuracy(tb, method = "wilson", digits = 3)
  object <- matrix(
    c(
      "0.884", "0.820", "0.927",
      "0.871", "0.766", "0.933",
      "0.938", "0.883", "0.968",
      "0.771", "0.660", "0.854",
      "6.851", "3.578", "13.118",
      "0.133", "0.083", "0.213"
    ),
    nrow = 6, byrow = TRUE,
    dimnames = list(
      c("sens", "spec", "ppv", "npv", "plr", "nlr"),
      ComparativeN = c("EST", "LowerCI", "UpperCI")
    )
  )
  expect_equal(res, as.data.frame(object))
})

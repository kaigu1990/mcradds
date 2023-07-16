test_that("diagTab function works as expected with default settings", {
  data("qualData")
  res <- qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
  expect_s4_class(res, "MCTab")
  object <- matrix(
    c(64, 11, 13, 112),
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
    c(112, 13, 11, 64),
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
      "0.8960", "0.8302", "0.9382",
      "0.8533", "0.7562", "0.9161",
      "0.9106", "0.8469", "0.9493",
      "0.8312", "0.7323", "0.8986",
      "6.1091", "3.5276", "10.5798",
      "0.1219", "0.0722", "0.2056"
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
      "0.8960", "0.8302", "0.9382",
      "0.8533", "0.7562", "0.9161",
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
    c(112, 13, 11, 64),
    nrow = 2, byrow = FALSE,
    dimnames = list(
      CandidateN = c("1", "0"),
      ComparativeN = c("1", "0")
    )
  )
  expect_equal(res@tab, object)
})

test_that("diagTab function works as expected with digits is 3", {
  data("qualData")
  tb <- qualData %>% diagTab(
    formula = ~ CandidateN + ComparativeN,
    rlevels = c(1, 0), clevels = c(1, 0)
  )
  res <- getAccuracy(tb, method = "wilson", digits = 3)
  object <- matrix(
    c(
      "0.896", "0.830", "0.938",
      "0.853", "0.756", "0.916",
      "0.911", "0.847", "0.949",
      "0.831", "0.732", "0.899",
      "6.109", "3.528", "10.580",
      "0.122", "0.072", "0.206"
    ),
    nrow = 6, byrow = TRUE,
    dimnames = list(
      c("sens", "spec", "ppv", "npv", "plr", "nlr"),
      ComparativeN = c("EST", "LowerCI", "UpperCI")
    )
  )
  expect_equal(res, as.data.frame(object))
})

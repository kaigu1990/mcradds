# diagTab ----

test_that("diagTab function works as expected with default settings", {
  data("qualData")
  res <- qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
  expect_s4_class(res, "MCTab")
  expect_equal(matrix(res@tab), matrix(c(54, 8, 16, 122)))
  expect_equal(res@levels, c("0", "1"))
})

test_that("diagTab function works as expected for qualitative performance", {
  data("qualData")
  res <- qualData %>%
    diagTab(
      formula = ~ CandidateN + ComparativeN,
      levels = c(1, 0)
    )
  expect_equal(matrix(res@tab), matrix(c(122, 16, 8, 54)))
})

test_that("diagTab function works as expected for within-reader performance", {
  data("PDL1RP")
  res <- PDL1RP$wtn_reader %>%
    diagTab(
      formula = Order ~ Value,
      bysort = "Sample",
      levels = c("Positive", "Negative"),
      rep = TRUE,
      across = "Site"
    )
  expect_equal(matrix(res@tab), matrix(c(203, 17, 7, 223)))
})

test_that("diagTab function works as expected for between-reader performance", {
  data("PDL1RP")
  res <- PDL1RP$btw_reader %>%
    diagTab(
      formula = Reader ~ Value,
      bysort = "Sample",
      levels = c("Positive", "Negative"),
      rep = TRUE,
      across = "Site"
    )
  expect_equal(matrix(res@tab), matrix(c(200, 15, 7, 228)))
})

test_that("diagTab function works as expected for between-site performance", {
  data("PDL1RP")
  res <- PDL1RP$btw_site %>%
    diagTab(
      formula = Site ~ Value,
      bysort = "Sample",
      levels = c("Positive", "Negative"),
      rep = TRUE
    )
  expect_equal(matrix(res@tab), matrix(c(201, 12, 4, 233)))
})

# getAccuracy ----

test_that("getAccuracy function works as expected with wilson method", {
  data("qualData")
  res <- qualData %>%
    diagTab(
      formula = ~ CandidateN + ComparativeN,
      levels = c(1, 0)
    ) %>%
    getAccuracy(ref = "r", r_ci = "wilson")

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

test_that("getAccuracy function works as expected without reference", {
  data("qualData")
  res <- qualData %>%
    diagTab(
      formula = ~ CandidateN + ComparativeN,
      levels = c(1, 0)
    ) %>%
    getAccuracy(ref = "nr", nr_ci = "wilson")

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

test_that("getAccuracy function works as expected with digits is 3", {
  data("qualData")
  res <- qualData %>%
    diagTab(
      formula = ~ CandidateN + ComparativeN,
      levels = c(1, 0)
    ) %>%
    getAccuracy(ref = "r", r_ci = "wilson", digits = 3)

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

test_that("getAccuracy function works as expected with between-readers precision", {
  data("PDL1RP")
  res <- PDL1RP$btw_reader %>%
    diagTab(
      formula = Reader ~ Value,
      bysort = "Sample",
      levels = c("Positive", "Negative"),
      rep = TRUE,
      across = "Site"
    ) %>%
    getAccuracy(ref = "bnr", rng.seed = 12306)

  object <- matrix(
    c(
      "0.9479", "0.9260", "0.9686",
      "0.9540", "0.9342", "0.9730",
      "0.9511", "0.9311", "0.9711"
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(
      c("apa", "ana", "opa"),
      ComparativeN = c("EST", "LowerCI", "UpperCI")
    )
  )
  expect_equal(res, as.data.frame(object))
})

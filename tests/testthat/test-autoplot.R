test_that("autoplot works as expected for BAsummary class with default arguments", {
  data("platelet")
  object <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)

  result1 <- autoplot(object, type = "absolute")
  vdiffr::expect_doppelganger("autoplot_BAsummary with absolute diff", result1)
  result2 <- autoplot(object, type = "relative")
  vdiffr::expect_doppelganger("autoplot_BAsummary with relative diff", result2)
})

test_that("autoplot works as expected for BAsummary class with multiple arguments", {
  data("platelet")
  object <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)

  result <- autoplot(
    object,
    type = "absolute",
    jitter = FALSE,
    fill = "lightblue",
    color = "grey",
    size = 2,
    ref.line.params = list(col = "grey"),
    loa.line.params = list(col = "grey"),
    label.digits = 2,
    label.params = list(col = "grey", size = 3, fontface = "italic"),
    x.nbreak = 6,
    main.title = "Bland-Altman Plot",
    x.title = "Mean of Test and Reference Methods",
    y.title = "Reference - Test"
  )
  vdiffr::expect_doppelganger("autoplot_BAsummary with multiple arguments", result)
})

test_that("autoplot works as expected for MCResult class with default arguments", {
  data(creatinine, package = "mcr")
  object <- mcreg2(
    x = platelet$Comparative, y = platelet$Candidate,
    method.reg = "Deming", method.ci = "jackknife"
  )

  result <- autoplot(object)
  vdiffr::expect_doppelganger("autoplot_MCResult with default arguments", result)
})

test_that("autoplot works as expected for MCResult class with multiple arguments", {
  data(creatinine, package = "mcr")
  object <- mcreg2(
    x = platelet$Comparative, y = platelet$Candidate,
    method.reg = "PaBa", method.ci = "bootstrap"
  )

  result <- autoplot(
    object,
    identity.params = list(col = "blue", linetype = "solid"),
    reg.params = list(col = "red", linetype = "solid"),
    equal.axis = TRUE,
    legend.title = FALSE,
    legend.digits = 3,
    x.title = "Reference",
    y.title = "Test"
  )
  vdiffr::expect_doppelganger("autoplot_MCResult with multiple arguments", result)
})

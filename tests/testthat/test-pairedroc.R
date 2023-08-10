test_that("aucTest works as expected with default settings", {
  data("ldlroc")
  res <- aucTest(x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis)
  expect_class(res, "tpROC")
  expect_identical(res@method, "difference")
  expect_identical(res@H0, 0)
  expect_equal(
    res@stat,
    list(
      diffauc = 0.2378247,
      se = 0.07904425,
      ci = c(0.08290079, 0.39274856),
      zstat = 3.008754,
      pval = 0.002623217
    ),
    tolerance = 0.0001
  )
})

test_that("aucTest works as expected when method is superiority", {
  data("ldlroc")
  res <- aucTest(
    x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
    method = "superiority", h0 = 0.1
  )
  expect_identical(res@method, "superiority")
  expect_identical(res@H0, 0.1)
  expect_equal(
    res@stat,
    list(
      diffauc = 0.2378247,
      se = 0.07904425,
      ci = c(0.08290079, 0.39274856),
      zstat = 1.743639,
      pval = 0.04061099
    ),
    tolerance = 0.0001
  )
})

test_that("aucTest works as expected when method is non-inferiority", {
  data("ldlroc")
  res <- aucTest(
    x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
    method = "non-inferiority", h0 = -0.1
  )
  expect_identical(res@method, "non-inferiority")
  expect_identical(res@H0, -0.1)
  expect_equal(
    res@stat,
    list(
      diffauc = 0.2378247,
      se = 0.07904425,
      ci = c(0.08290079, 0.39274856),
      zstat = 4.273868,
      pval = 9.605549e-06
    ),
    tolerance = 0.0001
  )
})

test_that("aucTest works as expected with specific arguments to keep silent", {
  data("ldlroc")
  res <- expect_silent(aucTest(
    x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
    levels = c(0, 1), direction = "<"
  ))
  object <- aucTest(
    x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis
  )
  expect_equal(res@stat, object@stat, tolerance = 0.0001)
})

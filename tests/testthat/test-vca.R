# anovaVCA ----

test_that("anovaVCA works as expetced", {
  data(glucose)
  fit <- anovaVCA(value ~ day / run, glucose)
  object <- matrix(
    c(
      64.77732, NA, NA, 12.933553, 100.00000, 3.596325, 1.4726965,
      19.00000, 415.8, 21.88421, 1.958553, 15.14319, 1.399483, 0.5730889,
      20.00000, 281.0, 14.05000, 3.075000, 23.77537, 1.753568, 0.7180867,
      40.00000, 316.0, 7.90000, 7.900000, 61.08144, 2.810694, 1.1509803
    ),
    byrow = TRUE, nrow = 4,
    dimnames = list(
      c("total", "day", "day:run", "error"),
      c("DF", "SS", "MS", "VC", "%Total", "SD", "CV[%]")
    )
  )
  expect_class(fit, "VCA")
  expect_equal(fit$aov.tab, object, tolerance = 0.0001)
})

# VCAinference ----

test_that("VCAinference works as expetced", {
  data(glucose)
  fit <- anovaVCA(value ~ day / run, glucose)
  ci <- VCAinference(fit)
  object <- data.frame(
    Name = c("total", "day", "day:run", "error"),
    LCL = c(1.2569983, NA, NA, 0.9449697),
    UCL = c(1.778450, NA, NA, 1.472683),
    row.names = c("total", "day", "day:run", "error")
  )
  expect_class(ci, "VCAinference")
  expect_equal(ci$ConfInt$CV$TwoSided, object, tolerance = 0.0001)
})

# descfreq ----

test_that("descfreq works as expected with specific format", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descfreq(
      var = "AGEGR1",
      bygroup = "TRTP",
      format = "xx (xx.x%)"
    )
  expect_class(res, "Desc")
  expect_identical(res@func, "descfreq")
  expect_identical(dim(res@mat), c(6L, 7L))
  expect_identical(dim(res@stat), c(3L, 4L))

  expect_equal(
    data.frame(res@mat[, c("n", "perc")]),
    data.frame(
      n = c(29, 10, 21, 45, 5, 10),
      perc = c(0.48333333, 0.16666667, 0.35000000, 0.75000000, 0.08333333, 0.16666667)
    )
  )

  object <- formatters::var_labels_remove(res@stat)
  expect_equal(
    data.frame(object),
    data.frame(
      VarName = c(rep("AGEGR1", 3)),
      Category = c("65-80", "<65", ">80"),
      Placebo = c("29 (48.3%)", "10 (16.7%)", "21 (35.0%)"),
      Xanomeline = c("45 (75.0%)", "5 (8.3%)", "10 (16.7%)")
    )
  )
})

test_that("descfreq works as expected with format of 'xx (xx.xx)' and na_str of '0'", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descfreq(
      var = "RACE",
      bygroup = "TRTP",
      format = "xx (xx.xx)",
      na_str = "0"
    )

  object <- formatters::var_labels_remove(res@stat)
  expect_equal(
    data.frame(object),
    data.frame(
      VarName = c(rep("RACE", 3)),
      Category = c("BLACK OR AFRICAN AMERICAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE"),
      Placebo = c("3 (0.05)", "57 (0.95)", "0"),
      Xanomeline = c("6 (0.10)", "53 (0.88)", "1 (0.02)")
    )
  )
})

test_that("descfreq works as expected with factor varibales of var argument", {
  data(adsl_sub)
  res <- adsl_sub %>%
    dplyr::mutate(
      AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
      SEX = factor(SEX, levels = c("M", "F")),
      RACE = factor(RACE, levels = c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN"))
    ) %>%
    descfreq(
      var = c("AGEGR1", "SEX", "RACE"),
      bygroup = "TRTP",
      format = "xx (xx.x%)",
      addtot = TRUE,
      na_str = "0"
    )
  expect_identical(dim(res@mat), c(24L, 7L))
  expect_identical(dim(res@stat), c(8L, 5L))
  expect_factor(
    res@mat$Category,
    levels = c(
      "<65", "65-80", ">80",
      "M", "F",
      "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN"
    )
  )

  object <- formatters::var_labels_remove(res@stat)[, c(1:2, 5)]
  expect_equal(
    data.frame(object),
    data.frame(
      VarName = c(rep("AGEGR1", 3), rep("SEX", 2), rep("RACE", 3)),
      Category = factor(
        c(
          "<65", "65-80", ">80",
          "M", "F",
          "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN"
        ),
        levels = c(
          "<65", "65-80", ">80",
          "M", "F",
          "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN"
        )
      ),
      Total = c(
        "15 (12.5%)", "74 (61.7%)", "31 (25.8%)",
        "51 (42.5%)", "69 (57.5%)",
        "110 (91.7%)", "1 (0.8%)", "9 (7.5%)"
      )
    )
  )
})

# descvar ----

test_that("descvar works as expected with default arguments", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descvar(
      var = "AGE",
      bygroup = "TRTP"
    )
  expect_class(res, "Desc")
  expect_identical(res@func, "descvar")
  expect_identical(dim(res@mat), c(12L, 4L))
  expect_identical(dim(res@stat), c(6L, 4L))

  expect_equal(
    as.data.frame(res@stat),
    data.frame(
      VarName = c(rep("AGE", 6)),
      label = c("N", "MEAN", "SD", "MEDIAN", "MAX", "MIN"),
      Placebo = c("60", "75.2", "8.96", "76.0", "89", "52"),
      Xanomeline = c("60", "74.6", "7.06", "75.5", "88", "56")
    )
  )
})

test_that("descvar works as expected with specific statistics and total column", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descvar(
      var = "AGE",
      bygroup = "TRTP",
      stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
      addtot = TRUE
    )
  expect_identical(dim(res@mat), c(15L, 4L))
  expect_identical(dim(res@stat), c(5L, 5L))

  expect_equal(
    as.data.frame(res@stat),
    data.frame(
      VarName = c(rep("AGE", 5)),
      label = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
      Placebo = c("60", "75.2 (8.96)", "76.0", "52, 89", "69.0, 83.0"),
      Xanomeline = c("60", "74.6 (7.06)", "75.5", "56, 88", "71.0, 79.0"),
      Total = c("120", "74.9 (8.04)", "76.0", "52, 89", "69.0, 81.0")
    )
  )
})

test_that("descvar works as expected with specified decimal of 2", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descvar(
      var = "BMIBL",
      bygroup = "TRTP",
      stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
      autodecimal = FALSE,
      decimal = 2,
      addtot = TRUE
    )
  expect_identical(dim(res@mat), c(15L, 4L))
  expect_identical(dim(res@stat), c(5L, 5L))

  expect_equal(
    as.data.frame(res@stat),
    data.frame(
      VarName = c(rep("BMIBL", 5)),
      label = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
      Placebo = c("60", "23.298 (3.6135)", "22.650", "15.10, 33.30", "21.050, 25.050"),
      Xanomeline = c("60", "25.742 (4.1310)", "25.250", "15.30, 34.50", "22.850, 28.050"),
      Total = c("120", "24.520 (4.0546)", "24.300", "15.10, 34.50", "21.800, 27.250")
    )
  )
})

test_that("descvar works as expected with multiple variables", {
  data(adsl_sub)
  res <- adsl_sub %>%
    descvar(
      var = c("AGE", "BMIBL"),
      bygroup = "TRTP",
      stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
      autodecimal = TRUE,
      addtot = TRUE
    )
  expect_identical(dim(res@mat), c(30L, 4L))
  expect_identical(dim(res@stat), c(10L, 5L))

  expect_equal(
    as.data.frame(res@stat),
    data.frame(
      VarName = c(rep("AGE", 5), rep("BMIBL", 5)),
      label = rep(c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"), 2),
      Placebo = c(
        "60", "75.2 (8.96)", "76.0", "52, 89", "69.0, 83.0",
        "60", "23.30 (3.614)", "22.65", "15.1, 33.3", "21.05, 25.05"
      ),
      Xanomeline = c(
        "60", "74.6 (7.06)", "75.5", "56, 88", "71.0, 79.0",
        "60", "25.74 (4.131)", "25.25", "15.3, 34.5", "22.85, 28.05"
      ),
      Total = c(
        "120", "74.9 (8.04)", "76.0", "52, 89", "69.0, 81.0",
        "120", "24.52 (4.055)", "24.30", "15.1, 34.5", "21.80, 27.25"
      )
    )
  )
})

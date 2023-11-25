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
      Total = c("15 (12.5%)", "74 (61.7%)", "31 (25.8%)",
                "51 (42.5%)", "69 (57.5%)",
                "110 (91.7%)", "1 (0.8%)", "9 (7.5%)")
    )
  )
})

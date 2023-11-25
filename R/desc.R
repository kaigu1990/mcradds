#' Summarize Frequency counts and percentages
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Create a summary table for one or more variables by one group, including a
#' total column if necessary.
#'
#' @param data (`data.frame`)\cr a data frame that contains the variables to be
#'  summarized and grouped.
#' @param denom (`data.frame`)\cr the denominator to use for the percentage, but
#'  not use temporarily. By default, it's NULL, meaning the function will use
#'  the number of values of the `data`, including missing value.
#' @param var (`vector`)\cr a character vector of variables to summarize within `data`.
#' @param bygroup (`string`)\cr a variable for grouping within `data`.
#' @param format (`string`)\cr formatting string from `formatters::list_valid_format_labels()`
#'  for frequency counts and percentages.
#' @param fctdrop (`logical`)\cr whether to include the levels of the variables
#'  but with no records.
#' @param addtot (`logical`)\cr whether to add total column in the output or not.
#' @param na_str (`string`)\cr a string to replace `NA` in the output if no records
#'  will be counted for any category.
#'
#' @note By default, the each category is sorted based on the corresponding factor
#'  level of `var` variable. If the variable is not a factor, that will be sorted
#'  alphabetically.
#'
#' @return A object `Desc` contains an intermediate data with long form for
#'  post-processing and final data with wide form for presentation.
#' @export
#'
#' @examples
#' data(adsl_sub)
#'
#' # Count the age group by treatment with 'xx (xx.x%)' format
#' adsl_sub %>%
#'   descfreq(
#'     var = "AGEGR1",
#'     bygroup = "TRTP",
#'     format = "xx (xx.x%)"
#'   )
#'
#' # Count the race by treatment with 'xx (xx.xx)' format and replace NA with '0'
#' adsl_sub %>%
#'   descfreq(
#'     var = "RACE",
#'     bygroup = "TRTP",
#'     format = "xx (xx.xx)",
#'     na_str = "0"
#'   )
#'
#' # Count the sex by treatment adding total column
#' adsl_sub %>%
#'   descfreq(
#'     var = "SEX",
#'     bygroup = "TRTP",
#'     format = "xx (xx.x%)",
#'     addtot = TRUE
#'   )
#'
#' # Count multiple variables by treatment and sort category by corresponding factor levels
#' adsl_sub %>%
#'   dplyr::mutate(
#'     AGEGR1 = factor(AGEGR1, levels = c("<65", "65-80", ">80")),
#'     SEX = factor(SEX, levels = c("M", "F")),
#'     RACE = factor(RACE, levels = c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN"))
#'   ) %>%
#'   descfreq(
#'     var = c("AGEGR1", "SEX", "RACE"),
#'     bygroup = "TRTP",
#'     format = "xx (xx.x%)",
#'     addtot = TRUE,
#'     na_str = "0"
#'   )
descfreq <- function(data,
                     denom = NULL,
                     var,
                     bygroup,
                     format,
                     fctdrop = FALSE,
                     addtot = FALSE,
                     na_str = NULL) {
  assert_data_frame(data)
  assert_subset(var, choices = names(data), empty.ok = FALSE)
  assert_subset(bygroup, choices = names(data), empty.ok = FALSE)
  assert_logical(fctdrop)
  assert_logical(addtot)
  assert_string(na_str, null.ok = TRUE)

  reslist <- lapply(var, function(x) {
    dat1 <- data %>%
      dplyr::count(!!rlang::sym(bygroup), !!rlang::sym(x), .drop = fctdrop) %>%
      dplyr::add_count(!!rlang::sym(bygroup), wt = n, name = "tot") %>%
      dplyr::mutate(
        perc = n / tot,
        VarName = x
      ) %>%
      dplyr::select(VarName, Category = !!rlang::sym(x), everything())

    dat2 <- if (addtot) {
      data %>%
        dplyr::count(!!rlang::sym(x), .drop = fctdrop) %>%
        dplyr::add_count(wt = n, name = "tot") %>%
        dplyr::mutate(
          perc = n / tot,
          VarName = x,
          !!rlang::sym(bygroup) := "Total"
        ) %>%
        dplyr::select(VarName, Category = !!rlang::sym(x), everything())
    }

    dat <- rbind(dat1, dat2)
  })
  df <- do.call(rbind, reslist)

  fmt_lst <- formatters::list_valid_format_labels()
  df$con <- if (format %in% fmt_lst$`1d`) {
    h_fmt_count_perc(df$n, format = format)
  } else if (format %in% fmt_lst$`2d`) {
    h_fmt_count_perc(df$n, perc = df$perc, format = format)
  } else {
    NA
  }

  tb <- df %>%
    tidyr::pivot_wider(
      id_cols = -c(n, tot, perc),
      names_from = !!rlang::sym(bygroup),
      values_from = con,
      values_fill = na_str
    )

  object <- Desc(
    func = "descfreq",
    mat = df,
    stat = tb
  )
  object
}

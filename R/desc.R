#' Summarize Frequency Counts and Percentages
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Create a summary table for one or more variables by one group, as well as a
#' total column if necessary.
#'
#' @param data (`data.frame`)\cr a data frame that contains the variables to be
#'  summarized and grouped.
#' @param denom (`data.frame`)\cr the denominator to use for the percentage, but
#'  not use temporarily. By default, it's NULL, meaning the function will use
#'  the number of values of the `data`, including missing value.
#' @param var (`vector`)\cr a character vector of variables to be summarized within `data`.
#' @param bygroup (`string`)\cr a character variable for grouping within `data`.
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
#'     RACE = factor(RACE, levels = c(
#'       "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE",
#'       "BLACK OR AFRICAN AMERICAN"
#'     ))
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
      dplyr::count(!!sym(bygroup), !!sym(x), .drop = fctdrop) %>%
      dplyr::add_count(!!sym(bygroup), wt = .data$n, name = "tot") %>%
      dplyr::mutate(
        perc = .data$n / .data$tot,
        VarName = x
      ) %>%
      dplyr::select("VarName", Category = !!sym(x), dplyr::everything())

    dat2 <- if (addtot) {
      data %>%
        dplyr::count(!!sym(x), .drop = fctdrop) %>%
        dplyr::add_count(wt = .data$n, name = "tot") %>%
        dplyr::mutate(
          perc = .data$n / .data$tot,
          VarName = x,
          !!sym(bygroup) := "Total"
        ) %>%
        dplyr::select("VarName", Category = !!sym(x), dplyr::everything())
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
      id_cols = -c("n", "tot", "perc"),
      names_from = !!sym(bygroup),
      values_from = "con",
      values_fill = na_str
    )

  object <- Desc(
    func = "descfreq",
    mat = df,
    stat = tb
  )
  object
}

#' Summarize Descriptive Statistics
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Create a summary table with a set of descriptive statistics for one or more
#' variables by one group, as well as a total column if necessary.
#'
#' @param data (`data.frame`)\cr a data frame that contains the variables to be
#'  summarized and grouped.
#' @param var (`vector`)\cr a character vector of variables to be summarized within `data`.
#' @param bygroup (`string`)\cr a character variable for grouping within `data`.
#' @param stats (`vector`)\cr a statistics character vector must be chosen from
#'  c("N", "MEAN", "SD", "MEDIAN", "MAX", "MIN", "Q1", "Q3", "MEANSD", "RANGE",
#'  "IQR", "MEDRANGE", "MEDIQR"), and the default are top six items.
#' @param autodecimal (`logical`)\cr whether to capture the variable's maximum
#'  decimal, and the final decimal precision is equal to the variable decimal plus
#'  the definition of each statistic from `getOption("mcradds.precision.default")`.
#' @param decimal (`integer`)\cr a integer number to define the decimal precision
#'  for each variable.
#' @param addtot (`logical`)\cr whether to add total column in the output or not.
#' @param .perctype (`integer`)\cr an integer between 1 and 9 selecting one of
#'  the nine quantile algorithms, also see the details in [quantile()]. The default
#'  is `2`, so that it can be consistent with SAS quantile calculation.
#'
#' @note The decimal precision is based on two aspects, one is the original precision
#'  from the variable or the `decimal` argument, and the second is the common use that
#'  has been defined in `getOption("mcradds.precision.default")`. So if you want to
#'  change the second decimal precision, you can alter it manually with `option()`.
#'
#' @return A object `Desc` contains an intermediate data with long form for
#'  post-processing and final data with wide form for presentation.
#' @export
#'
#' @examples
#' data(adsl_sub)
#'
#' # Compute the default statistics of AGE by TRTP group
#' adsl_sub %>%
#'   descvar(
#'     var = "AGE",
#'     bygroup = "TRTP"
#'   )
#'
#' # Compute the specific statistics of BMI by TRTP group, adding total column
#' adsl_sub %>%
#'   descvar(
#'     var = "BMIBL",
#'     bygroup = "TRTP",
#'     stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
#'     addtot = TRUE
#'   )
#'
#' # Set extra decimal to define precision
#' adsl_sub %>%
#'   descvar(
#'     var = "BMIBL",
#'     bygroup = "TRTP",
#'     stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
#'     autodecimal = FALSE,
#'     decimal = 2,
#'     addtot = TRUE
#'   )
#'
#' # Set multiple variables together
#' adsl_sub %>%
#'   descvar(
#'     var = c("AGE", "BMIBL", "HEIGHTBL"),
#'     bygroup = "TRTP",
#'     stats = c("N", "MEANSD", "MEDIAN", "RANGE", "IQR"),
#'     autodecimal = TRUE,
#'     addtot = TRUE
#'   )
descvar <- function(data,
                    var,
                    bygroup,
                    stats = getOption("mcradds.stats.default"),
                    autodecimal = TRUE,
                    decimal = 1,
                    addtot = FALSE,
                    .perctype = 2) {
  assert_data_frame(data)
  assert_subset(var, choices = names(data), empty.ok = FALSE)
  assert_subset(bygroup, choices = names(data), empty.ok = FALSE)
  assert_subset(stats, choices = c(
    "N", "MEAN", "SD", "MEDIAN", "MAX", "MIN", "Q1", "Q3",
    "MEANSD", "RANGE", "IQR", "MEDRANGE", "MEDIQR"
  ), empty.ok = FALSE)
  assert_logical(autodecimal)
  assert_int(decimal, lower = 0)
  assert_logical(addtot)
  assert_int(.perctype, lower = 1, upper = 9)

  if (addtot) {
    data <- data %>%
      dplyr::mutate(!!sym(bygroup) := "Total") %>%
      dplyr::bind_rows(., data)
  }

  precision_tb <- getOption("mcradds.precision.default")
  reslist <- lapply(var, function(x) {
    digit_ori <- sapply(data[[x]], function(v) {
      if (grepl("\\.", v)) {
        nchar(sub("[0-9]+\\.", "", v))
      } else {
        nchar(sub("[0-9]+", "", v))
      }
    }) %>% max()

    dig_tb <- if (autodecimal) {
      dplyr::mutate(precision_tb, digits = .data$extra + digit_ori) %>%
        as.data.frame()
    } else {
      dplyr::mutate(precision_tb, digits = .data$extra + decimal) %>%
        as.data.frame()
    }
    rownames(dig_tb) <- dig_tb$stat

    assert_numeric(data[[x]])
    data %>%
      dplyr::group_by(!!sym(bygroup)) %>%
      dplyr::summarise(
        N = as.character(sum(!is.na(!!sym(x)))),
        MEAN = formatC(
          mean(!!sym(x), na.rm = TRUE),
          format = "f",
          digits = dig_tb["MEAN", "digits"]
        ),
        SD = formatC(
          sd(!!sym(x), na.rm = TRUE),
          format = "f",
          digits = dig_tb["SD", "digits"]
        ),
        MEDIAN = formatC(
          median(!!sym(x), na.rm = TRUE),
          format = "f",
          digits = dig_tb["MEDIAN", "digits"]
        ),
        MAX = formatC(
          max(!!sym(x), na.rm = TRUE),
          format = "f",
          digits = dig_tb["MAX", "digits"]
        ),
        MIN = formatC(
          min(!!sym(x), na.rm = TRUE),
          format = "f",
          digits = dig_tb["MIN", "digits"]
        ),
        Q1 = formatC(
          quantile(!!sym(x), probs = 0.25, na.rm = TRUE, type = .perctype),
          format = "f",
          digits = dig_tb["Q1", "digits"]
        ),
        Q3 = formatC(
          quantile(!!sym(x), probs = 0.75, na.rm = TRUE, type = .perctype),
          format = "f",
          digits = dig_tb["Q3", "digits"]
        ),
        MEANSD = paste0(.data$MEAN, " (", .data$SD, ")"),
        RANGE = paste0(c(.data$MIN, .data$MAX), collapse = ", "),
        IQR = paste0(c(.data$Q1, .data$Q3), collapse = ", "),
        MEDRANGE = paste0(.data$MEDIAN, " (", .data$RANGE, ")"),
        MEDIQR = paste0(.data$MEDIAN, " (", .data$IQR, ")"),
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(VarName = x) %>%
      dplyr::select("VarName", !!sym(bygroup), dplyr::all_of(stats)) %>%
      dplyr::arrange(!!sym(bygroup) == "Total") %>%
      tidyr::pivot_longer(
        cols = -c(1:2),
        names_to = "label",
        values_to = "value"
      )
  })
  df <- do.call(rbind, reslist)

  tb <- df %>%
    tidyr::pivot_wider(
      id_cols = c("VarName", "label"),
      names_from = !!sym(bygroup),
      values_from = "value"
    )

  object <- Desc(
    func = "descvar",
    mat = df,
    stat = tb
  )
  object
}

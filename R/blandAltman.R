#' Calculate statistics for Bland-Altman
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Calculate the Bland-Altman related statistics with specific difference type,
#' such as difference, limited of agreement and confidence interval. And the outlier
#' detecting function and graphic function will get the difference result from this.
#'
#' @param x (`numeric`)\cr reference method.
#' @param y (`numeric`)\cr test method.
#' @param sid (`numeric` or `string`) sample id.
#' @param type1 (`integer`)\cr specifying a specific difference for absolute difference, default is 3.
#' @param type2 (`integer`)\cr specifying a specific difference for relative difference, default is 5.
#' @param conf.level (`numeric`)\cr significance level for two side, default is 0.95.
#'
#' @seealso [h_difference()] to see the type details.
#'
#' @return A object with [`BAsummary`] class that contains the BlandAltman analysis.
#' - `data` a data frame contains the raw data from the input.
#' - `stat` a list contains the summary table (`tab`) of Bland-Altman analysis,
#' vector (`absolute_diff`) of absolute difference and vector (`relative_diff`)
#' of relative difference.
#'
#' @export
#'
#' @examples
#' data("platelet")
#' blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#'
#' # with sample id as input sid
#' blandAltman(x = platelet$Comparative, y = platelet$Candidate, sid = platelet$Sample)
blandAltman <- function(x, y, sid = NULL, type1 = 3, type2 = 5, conf.level = 0.95) {
  assert_numeric(x)
  assert_numeric(x)(y)
  assert_choice(type1, choices = c(1, 3))
  assert_choice(type2, choices = c(2, 4, 5))
  assert_number(conf.level, lower = 0.7, upper = 1)

  data <- as.data.frame(
    if (is.null(sid)) {
      cbind(sid = 1:length(x), x, y)
    } else {
      cbind(sid = sid, x, y)
    }
  )

  abs_diff <- h_difference(x, y, type = type1)
  rel_diff <- h_difference(x, y, type = type2)

  abs_tb <- h_summarize(abs_diff[, "y_ba"], conf.level)
  rel_tb <- h_summarize(rel_diff[, "y_ba"], conf.level)
  ba_tab <- rbind(abs_tb, rel_tb)
  rownames(ba_tab) <- c("absolute_difference", "relative_difference")

  BAsummary(
    data = data,
    stat = list(
      tab = ba_tab,
      absolute_diff = abs_diff[, "y_ba"],
      relative_diff = rel_diff[, "y_ba"]
    )
  )
}

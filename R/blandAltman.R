#' Calculate statistics for Bland-Altman
#'
#' @param x (`numeric`)\cr reference method.
#' @param y (`numeric`)\cr test method.
#' @param sid (`numeric` or `string`) sample id.
#' @param type1 (`integer`)\cr specifying a specific difference for absolute difference, default is 3.
#' @param type2 (`integer`)\cr specifying a specific difference for relative difference, default is 5.
#' @param outlier (`logic`)\cr whether to calculate the outliers with 4E&4Er method.
#' @param confint (`numeric`)\cr significance level for two side, default is 0.95.
#'
#' @seealso [h_difference()] to see the type details.
#'
#' @return A object with [`BAsummary`] class that contains the BlandAltman analysis.
#' @export
#'
#' @examples
#' data("platelet")
#' blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#' data("creatinine", package = "mcr")
#' blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea, outlier = TRUE)
blandAltman <- function(x, y, sid = NULL, type1 = 3, type2 = 5, outlier = FALSE, confint = 0.95) {
  assert_vector(x)
  assert_vector(y)
  assert_choice(type1, choices = 1:5)
  assert_choice(type2, choices = 1:5)
  assert_logical(outlier)
  assert_numeric(confint, lower = 0.7, upper = 1)

  data <- as.data.frame(
    if (is.null(sid)) {
      cbind(sid = 1:length(x), x, y)
    } else {
      cbind(sid = sid, x, y)
    }
  )

  abs_diff <- h_difference(x, y, type = type1)
  rel_diff <- h_difference(x, y, type = type2)

  abs_tb <- h_summarize(abs_diff[, "y_ba"], confint)
  rel_tb <- h_summarize(rel_diff[, "y_ba"], confint)
  ba_tab <- rbind(abs_tb, rel_tb)
  rownames(ba_tab) <- c("absolute_difference", "relative_difference")

  if (outlier) {
    da <- abs_diff[, "y_ba"]
    ll_da <- abs_tb[1, "limit_lr"]
    lu_da <- abs_tb[1, "limit_up"]
    dr <- rel_diff[, "y_ba"]
    ll_dr <- rel_tb[1, "limit_lr"]
    lu_dr <- rel_tb[1, "limit_up"]
    outord <- which((da > lu_da | da < ll_da) & (dr > lu_dr | dr < ll_dr))
    outid <- data$sid[outord]
    outmat <- data[data$sid %in% outid, ]
    row.names(outmat) <- NULL
  }

  object <- BAsummary(
    data = data,
    stat = list(
      tab = ba_tab,
      absolute_diff = abs_diff[, "y_ba"],
      relative_diff = rel_diff[, "y_ba"]
    ),
    outlier = if (outlier) {
      list(ord = outord, sid = outid, mat = outmat)
    } else {
      list()
    }
  )
  object
}

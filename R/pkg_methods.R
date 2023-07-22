# show ----

#' Show Method for Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A show method that displays essential information of objects.
#'
#' @rdname show
#' @aliases show
#'
#' @param object (`any`)\cr input.
#'
#' @return None (invisible `NULL`), only used for the side effect of printing to
#'   the console.
#'
#' @importFrom purrr map_chr
#'
#' @examples
#' size_one_prop(p1 = 0.95, p0 = 0.9, alpha = 0.05, power = 0.8)
#' size_ci_corr(r = 0.9, lr = 0.85, alpha = 0.025, alternative = "greater")
setMethod(
  f = "show",
  signature = "SampleSize",
  definition = function(object) {
    cat_with_newline("\n", object@method, "\n")
    cat(" Call: ", append = FALSE)
    show(object@call)
    cat_with_newline("\n", "  optimal sample size: n =", ceiling(object@n), "\n")
    cat_with_newline(
      "  ",
      map_chr(seq_along(object@param), ~ paste(names(object@param[.x]),
        object@param[.x],
        sep = ":"
      ))
    )
  }
)

#' @rdname show
#' @aliases show
#'
#' @examples
#' qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
setMethod(
  f = "show",
  signature = "MCTab",
  definition = function(object) {
    cat(" Contingency Table: \n\n")
    cat_with_newline(
      "  candidate has", length(object@candidate$levels), "levels:",
      object@candidate$levels
    )
    cat_with_newline(
      "  comparative has", length(object@candidate$levels), "levels:",
      object@candidate$levels, "\n"
    )
    show(object@tab)
  }
)

#' @rdname show
#' @aliases show
#'
#' @examples
#' data("creatinine", package = "mcr")
#' blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea, outlier = TRUE)
setMethod(
  f = "show",
  signature = "BAsummary",
  definition = function(object) {
    df <- data.frame(object@stat$tab)

    N <- h_fmt_num(df$n, digits = 0, width = 1)
    mean_sd <- h_fmt_est(df$mean, df$sd, digits = c(3, 3), width = c(6, 6))
    median <- h_fmt_num(df$median, digits = 3, width = 1)
    q1_q3 <- h_fmt_range(df$q1, df$q3, digits = c(3, 3), width = c(6, 6))
    min_max <- h_fmt_range(df$min, df$max, digits = c(3, 3), width = c(6, 6))
    limit <- h_fmt_range(df$limit_lr, df$limit_up, digits = c(3, 3), width = c(6, 6))
    ci <- h_fmt_range(df$ci_lr, df$ci_up, digits = c(3, 3), width = c(6, 6))

    res <- rbind(N, mean_sd, median, q1_q3, min_max, limit, ci)
    row.names(res) <- c(
      "N", "Mean (SD)", "Median", "Q1, Q3", "Min, Max",
      "Limit of Agreement", "Confidence Interval of Mean"
    )
    colnames(res) <- c("Absolute difference", "Relative difference")

    print(data.frame(res))
  }
)

# getAccuracy ----

#' @rdname getAccuracy
#'
setGeneric("getAccuracy", function(object, ...) standardGeneric("getAccuracy"))

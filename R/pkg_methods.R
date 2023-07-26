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
#' blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea)
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
    limit <- h_fmt_range(df$limit_lr, df$limit_ur, digits = c(3, 3), width = c(6, 6))
    ci <- h_fmt_range(df$ci_lr, df$ci_ur, digits = c(3, 3), width = c(6, 6))

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


# getOutlier ----

#' @rdname getOutlier
#' @param ... additional arguments.
setGeneric("getOutlier", function(object, ...) standardGeneric("getOutlier"))

#' Detect Outliers From `BAsummary` Object
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Detect the potential outliers from the absolute and relative differences in
#' `BAsummary` object with 4E and ESD method.
#'
#' @note Bland-Altman analysis is used as the input data regardless of the 4E and ESD
#' method because it's necessary to determine the absolute and relative differences beforehand.
#' For the 4E method, both of the absolute and relative differences are required to
#' be define, and the bias exceeds the 4 fold of the absolute and relative differences.
#' However for the ESD method, only one of them is necessary (the latter is more recommended),
#' and the bias needs to meet the ESD test.
#'
#' @param object (`BAsummary`)\cr input from [blandAltman] function to generate the Bland-Altman
#' analysis result that contains the absolute and relative differences.
#' @param method (`string`)\cr string specifying which method to use. Default is `ESD`.
#' @param difference (`string`)\cr string specifying which difference type to use for `ESD` method.
#' Default is `abs` that means absolute difference, and `rel` is relative difference.
#' @param alpha (`numeric`)\cr type-I-risk. Only used when the method is defined as `ESD`.
#' @param h (`integer`)\cr the positive integer indicating the number of suspected outliers.
#' Only used when the method is defined as `ESD`.
#'
#' @rdname getOutlier
#' @aliases getOutlier
#'
#' @returns A list contains the statistics results (`stat`), outliers' ord id (`ord`),
#' sample id (`sid`), matrix with outliers (`outmat`) and matrix without outliers (`rmmat`).
#'
#' @export
#' @examples
#' data("platelet")
#' # Using `blandAltman` function with default arguments
#' ba <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#' getOutlier(ba, method = "ESD", difference = "rel")
#'
#' # Using sample id as input
#' ba2 <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, sid = platelet$Sample)
#' getOutlier(ba2, method = "ESD", difference = "rel")
#'
#' # Using `blandAltman` function when the `tyep2` is 2 with `X vs. (Y-X)/X` difference
#' ba3 <- blandAltman(x = platelet$Comparative, y = platelet$Candidate, type2 = 4)
#' getOutlier(ba3, method = "ESD", difference = "rel")
#'
#' # Using "4E" as the method input
#' data("creatinine", package = "mcr")
#' ba4 <- blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea)
#' getOutlier(ba4, method = "4E")
setMethod(
  f = "getOutlier",
  signature = c("BAsummary"),
  definition = function(object,
                        method = c("ESD", "4E"),
                        difference = c("abs", "rel"),
                        alpha = 0.05,
                        h = 5) {
    assert_class(object, "BAsummary")
    method <- match.arg(method, c("ESD", "4E"), several.ok = FALSE)
    assert_choice(method, c("ESD", "4E"))
    difference <- match.arg(difference, c("abs", "rel"), several.ok = FALSE)
    assert_choice(difference, c("abs", "rel"))
    assert_number(alpha, lower = 0, upper = 0.2)

    if (method == "4E") {
      stat <- data.frame(
        obs = 1:length(object@stat$absolute_diff),
        abs = object@stat$absolute_diff,
        abs_limit_lr = object@stat$tab[1, "limit_lr"],
        abs_limit_ur = object@stat$tab[1, "limit_ur"],
        rel = object@stat$relative_diff,
        rel_limit_lr = object@stat$tab[2, "limit_lr"],
        rel_limit_ur = object@stat$tab[2, "limit_ur"]
      )
      stat$Outlier <- with(
        stat,
        ifelse((abs > abs_limit_ur | abs < abs_limit_lr) &
          (rel > rel_limit_ur | rel < rel_limit_lr), TRUE, FALSE)
      )

      outord <- which(stat$Outlier == TRUE)
      if (length(outord) > 0) {
        rd <- object@data
        outid <- rd$sid[outord]
        outmat <- rd[rd$sid %in% outid, ]
        row.names(outmat) <- NULL
        rmout <- rd[!rd$sid %in% outid, ]
        row.names(rmout) <- NULL
        return(list(
          stat = stat[outord, ], ord = outord, sid = outid,
          outmat = outmat, rmmat = rmout
        ))
      } else {
        return(cat("No outlier is detected."))
      }
    }

    if (method == "ESD") {
      res <- if (difference == "abs") {
        ESD_test(object@stat$absolute_diff, alpha = alpha, h = h)
      } else if (difference == "rel") {
        ESD_test(object@stat$relative_diff, alpha = alpha, h = h)
      }

      if (length(res$ord) > 0) {
        rd <- object@data[complete.cases(object@data), ]
        outid <- rd$sid[res$ord]
        outmat <- rd[rd$sid %in% outid, ]
        row.names(outmat) <- NULL
        rmout <- rd[!rd$sid %in% outid, ]
        row.names(rmout) <- NULL
        return(c(res, list(sid = outid, outmat = outmat, rmmat = rmout)))
      } else {
        return(cat("No outlier is detected."))
      }
    }
  }
)

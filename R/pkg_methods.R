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
#' # Sample zie calculation
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
#'
#' # Get 2x2 Contingency Table
#' qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
setMethod(
  f = "show",
  signature = "MCTab",
  definition = function(object) {
    cat("Contingency Table: \n\n")
    cat_with_newline(
      "levels:",
      object@levels
    )
    show(object@tab)
  }
)

#' @rdname show
#' @aliases show
#'
#' @examples
#'
#' # Bland-Altman analysis
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

    if (object@param$type1 == 1) {
      typedes1 <- "Y-X"
    } else if (object@param$type1 == 3) {
      typedes1 <- "Y-X"
    }

    if (object@param$type2 == 2) {
      typedes2 <- "(Y-X)/X"
    } else if (object@param$type2 == 4) {
      typedes2 <- "(Y-X)/X"
    } else if (object@param$type2 == 5) {
      typedes2 <- "(Y-X)/(0.5*(X+Y))"
    }

    cat(" Call: ", append = FALSE)
    show(object@call)
    cat_with_newline("")
    cat_with_newline("  Absolute difference type: ", typedes1)
    cat_with_newline("  Relative difference type: ", typedes2)
    cat_with_newline("")
    print(data.frame(res))
  }
)

#' @rdname show
#' @aliases show
#'
#' @examples
#'
#' # Reference Interval
#' data("calcium")
#' refInterval(x = calcium$Value, RI_method = "nonparametric", CI_method = "nonparametric")
setMethod(
  f = "show",
  signature = "RefInt",
  definition = function(object) {
    cat_with_newline("\n", object@method, "\n")
    cat(" Call: ", append = FALSE)
    show(object@call)
    cat_with_newline("")
    cat_with_newline("  N =", object@n)
    cat_with_newline("  Outliers:", ifelse(length(object@outlier$out) == 0,
      "NULL", paste(object@outlier$out, collapse = " ")
    ))
    refint <- h_fmt_num(object@refInt, digits = 2, width = 2)
    cat_with_newline("  Reference Interval:", paste(refint, collapse = ", "))
    reflower <- h_fmt_num(object@confInt$refLower, digits = 4, width = 4)
    cat_with_newline("  RefLower Confidence Interval:", paste(reflower, collapse = ", "))
    refupper <- h_fmt_num(object@confInt$refUpper, digits = 4, width = 4)
    cat_with_newline("  Refupper Confidence Interval:", paste(refupper, collapse = ", "))
  }
)

#' @rdname show
#' @aliases show
#'
#' @examples
#'
#' # Comparing the Paired ROC when Non-inferiority margin <= -0.1
#' data("ldlroc")
#' aucTest(
#'   x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
#'   method = "non-inferiority", h0 = -0.1
#' )
setMethod(
  f = "show",
  signature = "tpROC",
  definition = function(object) {
    cat_with_newline(
      "\nThe hypothesis for testing", object@method,
      "based on Paired ROC curve\n"
    )
    cat_with_newline(" Test assay:")
    cat_with_newline(
      "  Area under the curve:",
      h_fmt_num(object@testROC$auc, digits = 4, width = 4)
    )
    cat_with_newline("  Standard Error(SE):", h_fmt_num(object@testROC$se, digits = 4, width = 4))
    cat_with_newline(
      "  95% Confidence Interval(CI):",
      paste(h_fmt_num(object@testROC$ci, digits = 4, width = 4), collapse = "-"),
      "(DeLong)"
    )
    cat_with_newline("\n Reference/standard assay:")
    cat_with_newline(
      "  Area under the curve:",
      h_fmt_num(object@refROC$auc, digits = 4, width = 4)
    )
    cat_with_newline("  Standard Error(SE):", h_fmt_num(object@refROC$se, digits = 4, width = 4))
    cat_with_newline(
      "  95% Confidence Interval(CI):",
      paste(h_fmt_num(object@refROC$ci, digits = 4, width = 4), collapse = "-"),
      "(DeLong)"
    )
    cat_with_newline("\n Comparison of Paired AUC:")
    cat_with_newline(
      "  Alternative hypothesis: the difference in AUC is", object@method,
      "to", object@H0
    )
    cat_with_newline(
      "  Difference of AUC:",
      h_fmt_num(object@stat$diffauc, digits = 4, width = 4)
    )
    cat_with_newline("  Standard Error(SE):", h_fmt_num(object@stat$se, digits = 4, width = 4))
    cat_with_newline(
      "  95% Confidence Interval(CI):",
      paste(h_fmt_num(object@stat$ci, digits = 4, width = 4), collapse = "-"),
      "(standardized differenec method)"
    )
    cat_with_newline("  Z:", h_fmt_num(object@stat$zstat, digits = 4, width = 4))
    cat_with_newline("  Pvalue:", formatC(object@stat$pval))
  }
)

# getAccuracy ----

#' @rdname getAccuracy
#'
setGeneric("getAccuracy", function(object, ...) standardGeneric("getAccuracy"))


# getOutlier ----

#' @rdname getOutlier
#' @param ... not used.
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

# autoplot ----

#' Generate a `ggplot` for Bland-Altman Plot and Regression Plot
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Draw a ggplot-based difference Bland-Altman plot of reference assay vs. test assay
#' for `BAsummary` object, and a regression plot for `MCResult`. Also Providing
#' the necessary and useful option arguments for presentation.
#'
#' @param object (`BAsummary`, `MCResult`)\cr input, depending on which function
#'  you have done, `blandAltman()` or `mcreg()`.
#' @param ... not used.
#'
#' @note If you'd like to alter any part that this `autoplot` function haven't
#'  provided, adding other `ggplot` statements are suggested.
#'
#' @return A `ggplot` based Bland-Altman plot or regression plot that can be
#'  easily customized using additional `ggplot` functions.
#'
#' @rdname autoplot
#' @aliases autoplot
#'
setGeneric("autoplot", function(object, ...) standardGeneric("autoplot"))

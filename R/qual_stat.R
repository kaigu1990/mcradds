#' @include pkg_methods.R
NULL

#' Creates Contingency Table
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Creates a 2x2 contingency table from the data frame or matrix for the qualitative
#' performance and reader precision of downstream analysis.
#'
#' @param formula (`numeric`)\cr a [formula] object with the cross-classifying
#'  variables (separated by `+`) on the right hand side. If data is wide structure,
#'  the row name of contingency is represented by the variable to the left of
#'  the `+` sign, and the col name is the right. If data is long structure, the
#'  classified variable is put on the left of the formula, and the value variable
#'  is put on the right.
#' @param data (`data.frame` or `matrix`)\cr a data frame or matrix.
#' @param bysort (`string`)\cr a sorted variable from the col names of `data`, and
#' a grouped variable for reproducibility analysis.
#' @param dimname (`vector`)\cr a character vector define the row name of contingency
#' table in first variable, and col name in second variable.
#' @param levels (`vector`)\cr a vector of known levels for measurements.
#' @param rep (`logical`)\cr weather to implement the reproducibility like reader
#' precision or not.
#'
#' @note
#' To be attention that if you would like to generate the 2x2 contingency table
#' for reproducibility analysis, the original data should be long structure and
#' corresponding formula.
#'
#' @return A object `matrix` contains the 2x2 contingency table.
#' @export
#'
#' @seealso [Summary()] for object to calculate diagnostic accuracy criteria.
#'
#' @examples
#' # For qualitative performance with wide data structure
#' data("qualData")
#' qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
#' qualData %>%
#'   diagTab(
#'     formula = ~ CandidateN + ComparativeN,
#'     levels = c(1, 0)
#'   )
#'
#' # For qualitative performance with long data structure
#' dummy <- data.frame(
#'   id = c("1001", "1001", "1002", "1002", "1003", "1003"),
#'   value = c(1, 0, 0, 0, 1, 1),
#'   type = c("Test", "Ref", "Test", "Ref", "Test", "Ref")
#' )
#' dummy %>%
#'   diagTab(
#'     formula = type ~ value,
#'     bysort = "id",
#'     dimname = c("Test", "Ref"),
#'     levels = c(1, 0)
#'   )
#'
#' # For reader precision performance in each site
#' data("PDL1RP")
#' reader <- PDL1RP$btw_reader
#' reader %>%
#'   diagTab(
#'     formula = Reader ~ Value,
#'     bysort = "Sample",
#'     levels = c("Positive", "Negative"),
#'     rep = TRUE,
#'     interrep = "Site"
#'   )
diagTab <- function(formula = ~.,
                    data,
                    bysort = NULL, dimname = NULL, levels = NULL,
                    rep = FALSE, interrep = NULL) {
  assert_formula(formula)
  assert_multi_class(data, classes = c("data.frame", "matrix"))
  assert_choice(bysort, names(data), null.ok = TRUE)

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  if (!is.null(bysort)) {
    data <- data[order(data[[bysort]]), ]
  }

  df <- stats::model.frame(formula, data)
  grpn <- attr(stats::terms(formula), "response")
  if (length(grpn) != 1) {
    stop("The left of ~ fomula should be only one variable.")
  }

  var <- attr(stats::terms(formula), "term.labels")
  if (grpn == 0) {
    if (length(var) == 2) {
      rdat <- h_factor(df, var = var[1], levels = levels)
      cdat <- h_factor(df, var = var[2], levels = levels)
      res <- table(rdat, cdat, dnn = c(var[1], var[2]))
    } else {
      stop("If the left of ~ formula is missing, the right of it must have two variables.")
    }
  } else {
    grp <- names(df)[grpn]
    df <- data[, c(interrep, names(df))]
    if (rep) {
      byinter <- split(df, as.formula(paste("~", interrep)))
      bylist <- lapply(byinter, function(x) {
        byname <- t(combn(unique(x[[grp]]), 2))
        do.call(rbind, lapply(1:nrow(byname), function(i) {
          cbind(
            value1 = x[[var]][x[[grp]] == byname[i, 1]],
            value2 = x[[var]][x[[grp]] == byname[i, 2]]
          )
        }))
      })
      bydf <- data.frame(do.call(rbind, bylist))
      rdat <- h_factor(bydf, var = "value1", levels = levels)
      cdat <- h_factor(bydf, var = "value2", levels = levels)
      res <- table(rdat, cdat, dnn = c("Pairwise1", "Pairwise2"))
    } else {
      dflist <- split(df, as.formula(paste("~", grp)))
      nms <- if (!is.null(dimname)) {
        dimname
      } else {
        names(dflist)
      }
      rdat <- h_factor(dflist[[nms[1]]], var = var, levels = levels)
      cdat <- h_factor(dflist[[nms[2]]], var = var, levels = levels)
      res <- table(rdat, cdat, dnn = nms)
    }
  }

  object <- MCTab(
    data = data,
    tab = res,
    levels = levels(rdat)
  )
  object
}

#' Summary Method for `MCTab` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Provides a concise summary of the content of [`MCTab`] objects. Computes
#' sensitivity, specificity, positive and negative predictive values and positive
#' and negative likelihood ratios for a diagnostic test with reference/gold standard.
#' Computes positive and negative percent agreement, and overall percent agreement
#' when the new test is evaluated by comparison to a non-reference standard.
#'
#' @param object (`MCTab`)\cr input from [diagTab] function to create 2x2 contingency table.
#' @param withref (`logical`)\cr weather the comparison test is reference/gold standard or not.
#' @param alpha (`numeric`)\cr type-I-risk, \eqn{\alpha}.
#' @param method (`string`)\cr string specifying the which method to use. Default is `wilson`.
#' Options can be `wilson`, `wald` and `clopper-pearson`, see [DescTools::BinomCI].
#' @param digits (`integer`)\cr the desired number of digits. Default is 4.
#' @param ... other arguments to be passed to [DescTools::BinomCI].
#'
#' @rdname getAccuracy
#' @aliases getAccuracy
#'
#' @returns A data frame contains the qualitative diagnostic accuracy criteria with
#' three columns for estimated value and confidence interval.
#' - sens: Sensitivity refers to how often the test is positive when the condition
#'  of interest is present.
#' - spec: Specificity refers to how often the test is negative when the condition
#'  of interest is absent.
#' - ppv: Positive predictive value refers to the percentage of subjects with
#'  a positive test result who have the target condition.
#' - npv: Negative predictive value refers to the percentage of subjects with
#'  a negative test result who do not have the target condition.
#' - plr: Positive likelihood ratio refers to the probability of true positive
#'  rate divided by the false negative rate.
#' - nlr: Negative likelihood ratio refers to the probability of false positive
#'  rate divided by the true negative rate.
#' - ppa: Positive percent agreement, equals to sensitivity when the candidate method
#'  is evaluated by comparison with a comparative method, not reference/gold standard.
#' - npa: Negative percent agreement, equals to specificity when the candidate method
#'  is evaluated by comparison with a comparative method, not reference/gold standard.
#'
#' @export
#' @examples
#' tab <- qualData %>%
#'   diagTab(
#'     formula = ~ CandidateN + ComparativeN,
#'     levels = c(1, 0)
#'   )
#' getAccuracy(tab, method = "wilson")
#' getAccuracy(tab, method = "wilson", withref = FALSE)
setMethod(
  f = "getAccuracy",
  signature = c("MCTab"),
  definition = function(object,
                        withref = TRUE,
                        method = c("wilson", "wald", "clopper-pearson"),
                        alpha = 0.05,
                        digits = 4,
                        ...) {
    assert_class(object, "MCTab")
    assert_logical(withref)
    method <- match.arg(method, c("wilson", "wald", "clopper-pearson"), several.ok = FALSE)
    assert_choice(method, c("wilson", "wald", "clopper-pearson"))


    tp <- object@tab[1, 1]
    fp <- object@tab[1, 2]
    fn <- object@tab[2, 1]
    tn <- object@tab[2, 2]
    n <- sum(object@tab)

    # sens (sensitivity)
    sens <- DescTools::BinomCI(x = tp, n = tp + fn, conf.level = 1 - alpha, method = method, ...)
    # spec (specificity)
    spec <- DescTools::BinomCI(x = tn, n = tn + fp, conf.level = 1 - alpha, method = method, ...)
    # PPV (positive predictive value)
    ppv <- DescTools::BinomCI(x = tp, n = tp + fp, conf.level = 1 - alpha, method = method, ...)
    # NPV (negative predictive value)
    npv <- DescTools::BinomCI(x = tn, n = tn + fn, conf.level = 1 - alpha, method = method, ...)

    # PLR (positive likelihood ratio)
    p1 <- sens[1, 1]
    p2 <- 1 - spec[1, 1]
    x1 <- tp
    x2 <- fp
    plr <- p1 / p2
    plr_ll <- plr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    plr_ul <- plr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    plr <- matrix(c(plr, plr_ll, plr_ul), byrow = FALSE, ncol = 3)

    # NLR(negative likelihood ratio)
    p1 <- 1 - sens[1, 1]
    p2 <- spec[1, 1]
    x1 <- fn
    x2 <- tn
    nlr <- p1 / p2
    nlr_ll <- nlr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    nlr_ul <- nlr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    nlr <- matrix(c(nlr, nlr_ll, nlr_ul), byrow = FALSE, ncol = 3)

    # PPA (Positive percent agreement)
    ppa <- DescTools::BinomCI(x = tp, n = tp + fn, conf.level = 0.95, method = "wilson", ...)

    # NPA (Negative percent agreement)
    npa <- DescTools::BinomCI(x = tn, n = tn + fp, conf.level = 0.95, method = "wilson", ...)

    # OPA (Overall percent agreement)
    opa <- DescTools::BinomCI(x = tp + tn, n = n, conf.level = 0.95, method = "wilson", ...)


    if (withref) {
      res <- rbind(sens, spec, ppv, npv, plr, nlr)
      row.names(res) <- c("sens", "spec", "ppv", "npv", "plr", "nlr")
    } else {
      res <- rbind(ppa, npa, opa)
      row.names(res) <- c("ppa", "npa", "opa")
    }
    colnames(res) <- c("EST", "LowerCI", "UpperCI")

    as.data.frame(formatC(res, digits = digits, format = "f"))
  }
)

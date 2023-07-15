#' @include pkg_methods.R
NULL

#' Creates Contingency Table
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Creates a 2x2 contingency table from the data frame or matrix for the qualitative
#' performance of downstream analysis.
#'
#' @param formula (`numeric`)\cr a [formula] object with the cross-classifying
#'  variables (separated by `+`) on the right hand side. The row name of contingency
#'  is represented by the variable to the left of the `+` sign, and the col name
#'  is represented by the variable to the right.
#' @param data (`data.frame` or `matrix`)\cr a data frame or matrix.
#' @param rlevels (`vector`)\cr a character vector of known levels for row name
#'  of contingency table.
#' @param clevels (`vector`)\cr a character vector of known levels for col name
#'  of contingency table.
#' @param ... other arguments to be passed to [xtabs()].
#'
#' @return A object `matrix` contains the 2x2 contingency table.
#' @export
#'
#' @seealso [Summary()] for object to calculate diagnosis performance.
#'
#' @examples
#' data("qualData")
#' qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
#' qualData %>%
#'   diagTab(
#'     formula = ~ CandidateN + ComparativeN,
#'     rlevels = c(1, 0), clevels = c(1, 0)
#'   )
diagTab <- function(formula = ~., data, ..., rlevels = NULL, clevels = NULL) {
  assert_formula(formula)
  assert_multi_class(data, classes = c("data.frame", "matrix"))

  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  df <- stats::model.frame(formula, data)
  var <- labels(terms(formula))
  df[[var[1]]] <- h_factor(df, var = var[1], levels = rlevels)
  df[[var[2]]] <- h_factor(df, var = var[2], levels = clevels)

  res <- stats::xtabs(formula = formula, data = df, ...)
  object <- MCTab(
    tab = as.matrix(res),
    candidate = list(
      data = df[[var[1]]],
      levels = levels(df[[var[1]]])
    ),
    comparative = list(
      data = df[[var[2]]],
      levels = levels(df[[var[2]]])
    )
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
#' @param method (`string`)\cr string specifying the which method to use, `wilson` is default.
#' Options can be `wilson`, `wald` and `clopper-pearson`, see [DescTools::BinomCIn].
#' @param ... other arguments to be passed to [DescTools::BinomCIn].
#'
#' @rdname summary
#'
#' @export
#' @examples
#' summary(m, method = "wilson")
#' summary(m, method = "wilson", withref = FALSE)
setMethod(
  f = "summary",
  signature = c("MCTab"),
  definition = function(object,
                        withref = TRUE,
                        method = c("wilson", "wald", "clopper-pearson"),
                        alpha = 0.05,
                        ...) {
    assert_class(object, "MCTab")
    assert_logical(withref)
    assert_choice(method, c("wilson", "wald", "clopper-pearson"))
    method <- match.arg(method, c("wilson", "wald", "clopper-pearson"), several.ok = FALSE)

    tp = object@tab[1,1]
    fp = object@tab[1,2]
    fn = object@tab[2,1]
    tn = object@tab[2,2]
    n = sum(object@tab)

    # sens
    sens <- DescTools::BinomCI(x = tp, n = tp + fn, conf.level = 1 - alpha, method = method, ...)
    # spec
    spec <- DescTools::BinomCI(x = tn, n = tn + fp, conf.level = 1 - alpha, method = method, ...)
    # PPV (positive predictive value)
    ppv <- DescTools::BinomCI(x = tp, n = tp + fp, conf.level = 1 - alpha, method = method, ...)
    # NPV (negative predictive value)
    npv <- DescTools::BinomCI(x = fn, n = fn + tn, conf.level = 1 - alpha, method = method, ...)

    # PLR (positive likelihood ratio)
    p1 <- sens[1,1]
    p2 <- 1 - spec[1,1]
    x1 <- tp
    x2 <- fp
    plr <- p1 / p2
    plr_ll <- plr * exp(-1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    plr_ul <- plr * exp(+1.96 * sqrt((1 - p1) / x1 + (1 - p2) / x2))
    plr <- matrix(c(plr, plr_ll, plr_ul), byrow = FALSE, ncol = 3)

    # NLR(negative likelihood ratio)
    p1 <- 1 - sens[1,1]
    p2 <- spec[1,1]
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
      rbind(sens, spec, ppv, npv, plr, nlr)
    } else {
      rbind(ppa, npa, opa)
    }

  }
)

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
#' @param rep (`logical`)\cr whether to implement the reproducibility like reader
#' precision or not.
#' @param across (`string`)\cr the across variable to split original data set to
#' subsets. The between-reader and within-reader precision's across variable
#' is `site` commonly.
#'
#' @note
#' To be attention that if you would like to generate the 2x2 contingency table
#' for reproducibility analysis, the original data should be long structure and
#' using the corresponding formula.
#'
#' @return A object `MCTab` contains the 2x2 contingency table.
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
#' # For Between-Reader precision performance
#' data("PDL1RP")
#' reader <- PDL1RP$btw_reader
#' reader %>%
#'   diagTab(
#'     formula = Reader ~ Value,
#'     bysort = "Sample",
#'     levels = c("Positive", "Negative"),
#'     rep = TRUE,
#'     across = "Site"
#'   )
diagTab <- function(formula = ~.,
                    data,
                    bysort = NULL, dimname = NULL, levels = NULL,
                    rep = FALSE, across = NULL) {
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
      dat <- data.frame(rdat, cdat)
      res <- table(rdat, cdat, dnn = c(var[1], var[2]))
    } else {
      stop("If the left of ~ formula is missing, the right of it must have two variables.")
    }
  } else {
    grp <- names(df)[grpn]
    if (!is.null(across)) {
      df <- data[, c(across, names(df))]
    }

    if (rep) {
      if (!is.null(across)) {
        byinter <- split(df, as.formula(paste("~", across)))
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
      } else {
        byname <- t(combn(unique(df[[grp]]), 2))
        bydf <- data.frame(
          do.call(rbind, lapply(1:nrow(byname), function(i) {
            cbind(
              value1 = df[[var]][df[[grp]] == byname[i, 1]],
              value2 = df[[var]][df[[grp]] == byname[i, 2]]
            )
          }))
        )
      }
      rdat <- h_factor(bydf, var = "value1", levels = levels)
      cdat <- h_factor(bydf, var = "value2", levels = levels)
      dat <- data.frame(rdat, cdat)
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
      dat <- data.frame(rdat, cdat)
      res <- table(rdat, cdat, dnn = nms)
    }
  }

  object <- MCTab(
    data = dat,
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
#' Computes positive/negative percent agreement, overall percent agreement and Kappa
#' when the new test is evaluated by comparison to a non-reference standard. Computes
#' average positive/negative agreement when the both tests are all not the
#' reference, such as paired reader precision.
#'
#' @param object (`MCTab`)\cr input from [diagTab] function to create 2x2 contingency table.
#' @param ref (`character`)\cr reference condition. It is possible to choose one
#' condition for your require. The `r` indicates that the comparative test is standard
#' reference, `nr` indicates the comparative test is not a standard reference, and
#' `bnr` indicates both the new test and comparative test are not references.
#' @param alpha (`numeric`)\cr type-I-risk, \eqn{\alpha}.
#' @param r_ci (`string`)\cr string specifying which method to calculate the
#' confidence interval for a diagnostic test with reference/gold standard. Default
#' is `wilson`. Options can be `wilson`, `wald` and `clopper-pearson`, see [DescTools::BinomCI].
#' @param nr_ci (`string`)\cr string specifying which method to calculate the
#' confidence interval for the comparative test with non-reference standard. Default
#' is `wilson`. Options can be `wilson`, `wald` and `clopper-pearson`, see [DescTools::BinomCI].
#' @param bnr_ci (`string`)\cr string specifying which method to calculate the
#' confidence interval for both tests are not reference like reader precision. Default
#' is `bootstrap`. But when the point estimate of `ANA` or `APA` is equal to 0 or 100%,
#' the method will be changed to `transformed wilson`.
#' @param bootCI (`string`)\cr string specifying the which bootstrap confidence
#' interval from `boot.ci()` function in `boot` package. Default is
#' `perc`(bootstrap percentile), options can be `norm`(normal approximation),
#' `boot`(basic bootstrap), `stud`(studentized bootstrap) and `bca`(adjusted
#' bootstrap percentile).
#' @param nrep (`integer`)\cr number of replicates for bootstrapping, default is 1000.
#' @param rng.seed (`integer`)\cr number of the random number generator seed
#' for bootstrap sampling. If set to NULL currently in the R session used RNG
#' setting will be used.
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
#' - opa: Overall percent agreement.
#' - kappa: Cohen's kappa coefficient to measure the level of agreement.
#' - apa: Average positive agreement refers to the positive agreements and can be
#'  regarded as weighted ppa.
#' - ana: Average negative agreement refers to the negative agreements and can be
#'  regarded as weighted npa.
#'
#' @export
#' @examples
#' # For qualitative performance
#' data("qualData")
#' tb <- qualData %>%
#'   diagTab(
#'     formula = ~ CandidateN + ComparativeN,
#'     levels = c(1, 0)
#'   )
#' getAccuracy(tb, ref = "r")
#' getAccuracy(tb, ref = "nr", nr_ci = "wilson")
#'
#' # For Between-Reader precision performance
#' data("PDL1RP")
#' reader <- PDL1RP$btw_reader
#' tb2 <- reader %>%
#'   diagTab(
#'     formula = Reader ~ Value,
#'     bysort = "Sample",
#'     levels = c("Positive", "Negative"),
#'     rep = TRUE,
#'     across = "Site"
#'   )
#' getAccuracy(tb2, ref = "bnr")
#' getAccuracy(tb2, ref = "bnr", rng.seed = 12306)
setMethod(
  f = "getAccuracy",
  signature = c("MCTab"),
  definition = function(object,
                        ref = c("r", "nr", "bnr"),
                        alpha = 0.05,
                        r_ci = c("wilson", "wald", "clopper-pearson"),
                        nr_ci = c("wilson", "wald", "clopper-pearson"),
                        bnr_ci = "bootstrap",
                        bootCI = c("perc", "norm", "basic", "stud", "bca"),
                        nrep = 1000,
                        rng.seed = NULL,
                        digits = 4,
                        ...) {
    assert_class(object, "MCTab")
    assert_numeric(alpha, lower = 0, upper = 0.2)
    ref <- match.arg(ref, c("r", "nr", "bnr"), several.ok = FALSE)
    assert_choice(ref, c("r", "nr", "bnr"))
    r_ci <- match.arg(r_ci, c("wilson", "wald", "clopper-pearson"), several.ok = FALSE)
    assert_choice(r_ci, c("wilson", "wald", "clopper-pearson"))
    nr_ci <- match.arg(nr_ci, c("wilson", "wald", "clopper-pearson"), several.ok = FALSE)
    assert_choice(nr_ci, c("wilson", "wald", "clopper-pearson"))
    bootCI <- match.arg(bootCI, c("perc", "norm", "basic", "stud", "bca"), several.ok = FALSE)
    assert_choice(bootCI, c("perc", "norm", "basic", "stud", "bca"))


    tp <- object@tab[1, 1]
    fp <- object@tab[1, 2]
    fn <- object@tab[2, 1]
    tn <- object@tab[2, 2]
    n <- sum(object@tab)

    if (ref == "r") {
      # sens (sensitivity)
      sens <- DescTools::BinomCI(
        x = tp, n = tp + fn,
        conf.level = 1 - alpha,
        method = r_ci, ...
      )
      # spec (specificity)
      spec <- DescTools::BinomCI(
        x = tn, n = tn + fp,
        conf.level = 1 - alpha,
        method = r_ci, ...
      )
      # PPV (positive predictive value)
      ppv <- DescTools::BinomCI(
        x = tp, n = tp + fp,
        conf.level = 1 - alpha,
        method = r_ci, ...
      )
      # NPV (negative predictive value)
      npv <- DescTools::BinomCI(
        x = tn, n = tn + fn,
        conf.level = 1 - alpha,
        method = r_ci, ...
      )

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

      res <- rbind(sens, spec, ppv, npv, plr, nlr)
      row.names(res) <- c("sens", "spec", "ppv", "npv", "plr", "nlr")
    } else if (ref == "nr") {
      # PPA (Positive percent agreement)
      ppa <- DescTools::BinomCI(
        x = tp, n = tp + fn,
        conf.level = 1 - alpha,
        method = nr_ci, ...
      )

      # NPA (Negative percent agreement)
      npa <- DescTools::BinomCI(
        x = tn, n = tn + fp,
        conf.level = 1 - alpha,
        method = nr_ci, ...
      )

      # OPA (Overall percent agreement)
      opa <- DescTools::BinomCI(
        x = tp + tn, n = n,
        conf.level = 1 - alpha,
        method = nr_ci, ...
      )

      # Cohen's Kappa for two rates
      kappa <- CohenKappa(object@tab, weights = "Unweighted", conf.level = 1 - alpha)

      res <- rbind(ppa, npa, opa, kappa)
      row.names(res) <- c("ppa", "npa", "opa", "kappa")
    } else if (ref == "bnr") {
      if (!is.null(rng.seed)) {
        set.seed(rng.seed)
      }
      boot_func <- function(data, ind) {
        tab <- table(data[ind, "rdat"], data[ind, "cdat"])

        # APA (Average positive agreement)
        apa <- 2 * tab[1, 1] / (2 * tab[1, 1] + tab[1, 2] + tab[2, 1])
        # ANA (Average negative agreement)
        ana <- 2 * tab[2, 2] / (2 * tab[2, 2] + tab[1, 2] + tab[2, 1])
        # OPA (Overall percent agreement)
        opa <- (tab[1, 1] + tab[2, 2]) / sum(tab)

        c(apa, ana, opa)
      }
      sim <- boot(data = object@data, statistic = boot_func, R = nrep)

      if (is.na(sim$t0[1])) {
        ci1 <- c(NA, NA)
      } else if (sim$t0[1] %in% c(0, 100)) {
        apa_temp <- DescTools::BinomCI(
          x = tp, n = tp + fp + fn,
          conf.level = 1 - alpha,
          method = "wilson", ...
        )
        ci1 <- c(
          2 * apa_temp[1, 2] / (1 + apa_temp[1, 2]),
          2 * apa_temp[1, 3] / (1 + apa_temp[1, 3])
        )
      } else {
        bci1 <- boot.ci(sim, conf = 1 - alpha, type = bootCI, index = 1)
        bootlen <- length(bci1[[4]])
        ci1 <- c(bci1[[4]][bootlen - 1], bci1[[4]][bootlen])
      }

      if (is.na(sim$t0[2])) {
        ci2 <- c(NA, NA)
      } else if (sim$t0[2] %in% c(0, 100)) {
        ana_temp <- DescTools::BinomCI(
          x = tn, n = tn + fp + fn,
          conf.level = 1 - alpha,
          method = "wilson", ...
        )
        ci2 <- c(
          2 * apa_temp[1, 2] / (1 + apa_temp[1, 2]),
          2 * apa_temp[1, 3] / (1 + apa_temp[1, 3])
        )
      } else {
        bci2 <- boot.ci(sim, conf = 1 - alpha, type = bootCI, index = 2)
        bootlen <- length(bci2[[4]])
        ci2 <- c(bci2[[4]][bootlen - 1], bci2[[4]][bootlen])
      }

      if (sim$t0[3] %in% c(0, 100)) {
        opa_temp <- DescTools::BinomCI(
          x = tp + tn, n = n,
          conf.level = 1 - alpha,
          method = "wilson", ...
        )
        ci3 <- c(opa_temp[1, 2], opa_temp[1, 3])
      } else {
        bci3 <- boot.ci(sim, conf = 1 - alpha, type = bootCI, index = 3)
        bootlen <- length(bci2[[4]])
        ci3 <- c(bci3[[4]][bootlen - 1], bci3[[4]][bootlen])
      }

      res <- cbind(sim$t0, rbind(ci1, ci2, ci3))
      row.names(res) <- c("apa", "ana", "opa")
    }

    colnames(res) <- c("EST", "LowerCI", "UpperCI")
    as.data.frame(formatC(res, digits = digits, format = "f"))
  }
)

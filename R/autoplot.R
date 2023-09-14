#' @include pkg_methods.R
NULL

#' @rdname autoplot
#'
#' @param object (`BAsummary`)\cr input.
#' @param type (`string`)\cr difference type from input, default is 'absolute'.
#' @param color,fill (`string`)\cr point colors.
#' @param size (`numeric`)\cr the size of points.
#' @param shape (`integer`)\cr the `ggplot` shape of points.
#' @param jitter (`logical`)\cr whether to add a small amount of random variation
#'  to the location of points.
#' @param ref.line (`logical`)\cr whether to plot a 'mean' line, default is TRUE.
#' @param ci.line (`logical`)\cr whether to plot a confidence interval line of 'mean',
#'  default is FALSE.
#' @param loa.line (`logical`)\cr whether to plot limit of agreement line, default is TRUE.
#' @param ref.line.params,ci.line.params,loa.line.params (`list`)\cr parameters
#'  (color, linetype, size) for the argument 'ref.line', 'ci.line' and 'loa.line';
#'  eg. ref.line.params = list(col = "blue", linetype = "solid", size = 1).
#' @param label (`logical`)\cr whether to add specific value label for each line
#'  (ref.line, ci.line and loa.line). Only be shown when the line is defined as TRUE.
#' @param label.digits (`integer`)\cr the number of digits after the decimal point.
#' @param label.params (`list`)\cr parameters (color, size, fontface) for the
#'  argument 'label'.
#' @param x.nbreak,y.nbreak (`integer`)\cr an integer guiding the number of major
#'  breaks of X/Y axis.
#' @param x.title,y.title,main.title (`string`)\cr the x axis, y axis and main
#'  title of plot.
#'
#' @seealso [h_difference()] to see the type details.
#'
#' @note If you'd like to alter any part that this `autoplot` function haven't
#'  provided, adding other `ggplot` statements are suggested.
#'
#' @return A `ggplot` based Bland-Altman plot that can be easily customized using
#' additional `ggplot` functions.
#'
#' @export
#'
#' @examples
#' # Using creatinine dataset from `mcr` package
#' data(creatinine, package = "mcr")
#' object <- blandAltman(x = creatinine$serum.crea, y = creatinine$plasma.crea)
#' autoplot(object)
#' autoplot(object, type = "relative")
#'
#' # Specify the type for difference plot
#' data("platelet")
#' object <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#' autoplot(object, type = "absolute")
#' autoplot(object, type = "relative")
#'
#' # Set the addition parameters for `geom_point`
#' autoplot(object,
#'   type = "relative",
#'   jitter = TRUE,
#'   fill = "lightblue",
#'   color = "grey",
#'   size = 2
#' )
#'
#' # Set the color and line type for reference and limits of agreement lines
#' autoplot(object,
#'   type = "relative",
#'   ref.line.params = list(col = "red", linetype = "solid"),
#'   loa.line.params = list(col = "grey", linetype = "solid")
#' )
#'
#' # Set label color, size and digits
#' autoplot(object,
#'   type = "absolute",
#'   ref.line.params = list(col = "grey"),
#'   loa.line.params = list(col = "grey"),
#'   label.digits = 2,
#'   label.params = list(col = "grey", size = 3, fontface = "italic")
#' )
#'
#' # Add main title, X and Y axis titles, and adjust X ticks.
#' autoplot(object,
#'   type = "absolute",
#'   x.nbreak = 6,
#'   main.title = "Bland-Altman Plot",
#'   x.title = "Mean of Test and Reference Methods",
#'   y.title = "Reference - Test"
#' )
setMethod(
  f = "autoplot",
  signature = c("BAsummary"),
  definition = function(object,
                        type = c("absolute", "relative"),
                        color = "black",
                        fill = "lightgray",
                        size = 1.5,
                        shape = 21,
                        jitter = FALSE,
                        ref.line = TRUE,
                        ref.line.params = list(col = "blue", linetype = "solid", size = 1),
                        ci.line = FALSE,
                        ci.line.params = list(col = "blue", linetype = "dashed"),
                        loa.line = TRUE,
                        loa.line.params = list(col = "blue", linetype = "dashed"),
                        label = TRUE,
                        label.digits = 4,
                        label.params = list(col = "black", size = 4),
                        x.nbreak = NULL,
                        y.nbreak = NULL,
                        x.title = NULL,
                        y.title = NULL,
                        main.title = NULL) {
    assert_class(object, "BAsummary")
    type <- match.arg(type, c("absolute", "relative"), several.ok = FALSE)
    assert_character(color)
    assert_character(fill)
    assert_number(size)
    assert_int(shape)
    assert_logical(jitter)
    assert_logical(ref.line)
    assert_logical(ci.line)
    assert_logical(loa.line)
    assert_logical(label)
    assert_int(label.digits)
    assert_int(x.nbreak, null.ok = TRUE)
    assert_int(x.nbreak, null.ok = TRUE)
    assert_subset(names(ref.line.params), c("col", "linetype", "size"))
    assert_subset(names(ci.line.params), c("col", "linetype", "size"))
    assert_subset(names(loa.line.params), c("col", "linetype", "size"))
    assert_subset(names(label.params), c("col", "size", "fontface"))

    x <- object@data$x
    y <- object@data$y
    sid <- object@data$sid
    statmat <- object@stat$tab[paste0(type, "_difference"), ]

    if (type == "absolute") {
      if (object@param$type1 == 1) {
        xaxis <- x
        x.title <- ifelse(is.null(x.title), "X", x.title)
      } else if (object@param$type1 == 3) {
        xaxis <- 0.5 * (x + y)
        x.title <- ifelse(is.null(x.title), "(X + Y) / 2", x.title)
      }
      yaxis <- y - x
      y.title <- ifelse(is.null(y.title), "Y - X", y.title)

      if (!all(yaxis == object@stat$absolute_diff, na.rm = TRUE)) {
        stop("An internal test in case that the difference is not equal to the results from `blandAltman` function")
      }
    } else if (type == "relative") {
      if (object@param$type2 == 2) {
        xaxis <- x
        x.title <- ifelse(is.null(x.title), "X", x.title)
      } else if (object@param$type2 %in% c(4, 5)) {
        xaxis <- 0.5 * (x + y)
        x.title <- ifelse(is.null(x.title), "(X + Y) / 2", x.title)
      }

      if (object@param$type2 %in% c(2, 4)) {
        yaxis <- (y - x) / x
        y.title <- ifelse(is.null(y.title), "(Y - X) / X", y.title)
      } else if (object@param$type2 == 5) {
        yaxis <- (y - x) / (0.5 * (x + y))
        y.title <- ifelse(is.null(y.title), "(Y - X) / ((X + Y) / 2)", y.title)
      }
      if (!all(yaxis == object@stat$relative_diff, na.rm = TRUE)) {
        stop("An internal test in case that the difference is not equal to the results from `blandAltman` function")
      }
    }

    df <- data.frame(sid = sid, xaxis = xaxis, yaxis = yaxis) %>% na.omit()
    yrange <- range(df["yaxis"])
    mgn <- (yrange[2] - yrange[1]) / 50
    xrange <- range(df["xaxis"])

    p <- ggplot(data = df, aes(x = xaxis, y = yaxis))

    if (jitter) {
      p <- p + geom_point(
        position = "jitter", color = color, fill = fill,
        size = size, shape = shape
      )
    } else {
      p <- p + geom_point(
        color = color, fill = fill,
        size = size, shape = shape
      )
    }

    if (ref.line) {
      p <- p +
        geom_hline(
          yintercept = statmat["mean"],
          col = if (is.null(ref.line.params[["col"]])) 1 else ref.line.params[["col"]],
          linetype = if (is.null(ref.line.params[["linetype"]])) 1 else ref.line.params[["linetype"]],
          size = if (is.null(ref.line.params[["size"]])) 0.9 else ref.line.params[["size"]]
        )

      if (label) {
        p <- p +
          geom_text(
            x = Inf, y = statmat[["mean"]] + mgn, hjust = 1,
            label = paste0("Mean = ", formatC(statmat[["mean"]], format = "f", label.digits)),
            col = if (is.null(label.params[["col"]])) 1 else label.params[["col"]],
            size = if (is.null(label.params[["size"]])) 4 else label.params[["size"]],
            fontface = if (is.null(label.params[["fontface"]])) "plain" else label.params[["fontface"]]
          )
      }
    }

    if (ci.line) {
      p <- p +
        geom_hline(
          yintercept = statmat["ci_lr"],
          col = if (is.null(ci.line.params[["col"]])) 1 else ci.line.params[["col"]],
          linetype = if (is.null(ci.line.params[["linetype"]])) 1 else ci.line.params[["linetype"]],
          size = if (is.null(ci.line.params[["size"]])) 0.8 else ci.line.params[["size"]]
        ) +
        geom_hline(
          yintercept = statmat["ci_ur"],
          col = if (is.null(ci.line.params[["col"]])) 1 else ci.line.params[["col"]],
          linetype = if (is.null(ci.line.params[["linetype"]])) 1 else ci.line.params[["linetype"]],
          size = if (is.null(ci.line.params[["size"]])) 0.8 else ci.line.params[["size"]]
        )

      if (label) {
        p <- p +
          geom_text(
            x = Inf, y = statmat[["ci_lr"]] + mgn, hjust = 1,
            label = paste0("Lower CI = ", formatC(statmat[["ci_lr"]], format = "f", label.digits)),
            col = if (is.null(label.params[["col"]])) 1 else label.params[["col"]],
            size = if (is.null(label.params[["size"]])) 4 else label.params[["size"]],
            fontface = if (is.null(label.params[["fontface"]])) "plain" else label.params[["fontface"]]
          ) +
          geom_text(
            x = Inf, y = statmat[["ci_ur"]] + mgn, hjust = 1,
            label = paste0("Upper CI = ", formatC(statmat[["ci_ur"]], format = "f", label.digits)),
            col = if (is.null(label.params[["col"]])) 1 else label.params[["col"]],
            size = if (is.null(label.params[["size"]])) 4 else label.params[["size"]],
            fontface = if (is.null(label.params[["fontface"]])) "plain" else label.params[["fontface"]]
          )
      }
    }

    if (loa.line) {
      p <- p +
        geom_hline(
          yintercept = statmat["limit_lr"],
          col = if (is.null(loa.line.params[["col"]])) 1 else loa.line.params[["col"]],
          linetype = if (is.null(loa.line.params[["linetype"]])) 1 else loa.line.params[["linetype"]],
          size = if (is.null(loa.line.params[["size"]])) 0.8 else loa.line.params[["size"]]
        ) +
        geom_hline(
          yintercept = statmat["limit_ur"],
          col = if (is.null(loa.line.params[["col"]])) 1 else loa.line.params[["col"]],
          linetype = if (is.null(loa.line.params[["linetype"]])) 1 else loa.line.params[["linetype"]],
          size = if (is.null(loa.line.params[["size"]])) 0.8 else loa.line.params[["size"]]
        )

      if (label) {
        p <- p +
          geom_text(
            x = Inf, y = statmat[["limit_lr"]] + mgn, hjust = 1,
            label = paste0("Lower LoA = ", formatC(statmat[["limit_lr"]], format = "f", label.digits)),
            col = if (is.null(label.params[["col"]])) 1 else label.params[["col"]],
            size = if (is.null(label.params[["size"]])) 4 else label.params[["size"]],
            fontface = if (is.null(label.params[["fontface"]])) "plain" else label.params[["fontface"]]
          ) +
          geom_text(
            x = Inf, y = statmat[["limit_ur"]] + mgn, hjust = 1,
            label = paste0("Upper LoA = ", formatC(statmat[["limit_ur"]], format = "f", label.digits)),
            col = if (is.null(label.params[["col"]])) 1 else label.params[["col"]],
            size = if (is.null(label.params[["size"]])) 4 else label.params[["size"]],
            fontface = if (is.null(label.params[["fontface"]])) "plain" else label.params[["fontface"]]
          )
      }
    }

    p <- p +
      ggplot2::labs(x = x.title, y = y.title, title = main.title) +
      scale_x_continuous(
        limits = c(0, xrange[2] + diff(xrange) / 50),
        breaks = if (is.null(x.nbreak)) scales::pretty_breaks(6) else waiver(),
        n.breaks = x.nbreak
      ) +
      scale_y_continuous(
        limits = c(c(-1, 1) * (max(abs(yrange)) + diff(yrange) / 50)),
        breaks = if (is.null(y.nbreak)) scales::pretty_breaks(6) else waiver(),
        n.breaks = y.nbreak
      )

    p <- p +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12, margin = margin(c(5, 0, 0, 0))),
        axis.title.y = element_text(size = 12, margin = margin(c(0, 5, 0, 0))),
        plot.margin = margin(c(15, 15, 10, 10))
      )
    p
  }
)

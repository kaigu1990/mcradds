#' @rdname autoplot
#'
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
#'  (color, linetype, linewidth) for the argument 'ref.line', 'ci.line' and 'loa.line';
#'  eg. ref.line.params = list(col = "blue", linetype = "solid", linewidth = 1).
#' @param label (`logical`)\cr whether to add specific value label for each line
#'  (ref.line, ci.line and loa.line). Only be shown when the line is defined as TRUE.
#' @param label.digits (`integer`)\cr the number of digits after the decimal point
#'  in the each label.
#' @param label.params (`list`)\cr parameters (color, size, fontface) for the
#'  argument 'label'.
#' @param x.nbreak,y.nbreak (`integer`)\cr an integer guiding the number of major
#'  breaks of x-axis and y-axis.
#' @param x.title,y.title,main.title (`string`)\cr the x-axis, y-axis and main
#'  title of plot.
#'
#' @seealso [h_difference()] to see the type details.
#'
#' @export
#'
#' @examples
#' # Specify the type for difference plot
#' data("platelet")
#' object <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#' autoplot(object)
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
          linewidth = if (is.null(ref.line.params[["linewidth"]])) 0.9 else ref.line.params[["linewidth"]]
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
          linewidth = if (is.null(ci.line.params[["linewidth"]])) 0.8 else ci.line.params[["size"]]
        ) +
        geom_hline(
          yintercept = statmat["ci_ur"],
          col = if (is.null(ci.line.params[["col"]])) 1 else ci.line.params[["col"]],
          linetype = if (is.null(ci.line.params[["linetype"]])) 1 else ci.line.params[["linetype"]],
          linewidth = if (is.null(ci.line.params[["linewidth"]])) 0.8 else ci.line.params[["linewidth"]]
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
          linewidth = if (is.null(loa.line.params[["linewidth"]])) 0.8 else loa.line.params[["linewidth"]]
        ) +
        geom_hline(
          yintercept = statmat["limit_ur"],
          col = if (is.null(loa.line.params[["col"]])) 1 else loa.line.params[["col"]],
          linetype = if (is.null(loa.line.params[["linetype"]])) 1 else loa.line.params[["linetype"]],
          linewidth = if (is.null(loa.line.params[["linewidth"]])) 0.8 else loa.line.params[["linewidth"]]
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

    p +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12, margin = margin(c(5, 0, 0, 0))),
        axis.title.y = element_text(size = 12, margin = margin(c(0, 5, 0, 0))),
        plot.margin = margin(c(15, 15, 10, 10))
      )
  }
)

#' @rdname autoplot
#'
#' @param identity (`logical`)\cr whether to add identity line, default is TRUE.
#' @param reg (`logical`)\cr whether to add regression line where the slope and
#'  intercept are obtained from [mcr::mcreg()] function, default is TRUE.
#' @param identity.params,reg.params (`list`)\cr parameters (color, linetype)
#'  for the argument 'identity' and 'reg'; eg. identity.params = list(col = "gray",
#'  linetype = "dashed").
#' @param equal.axis (`logical`)\cr whether to adjust the ranges of x-axis and y-axis
#'  are identical. If `equal.axis = TRUE`, x-axis will be equal to y-axis.
#' @param legend.title (`logical`)\cr whether to present the title in the legend.
#' @param legend.digits (`integer`)\cr the number of digits after the decimal point
#'  in the legend.
#'
#' @seealso [mcr::mcreg()] to see the regression parameters.
#'
#' @export
#'
#' @examples
#' # Using the default arguments for regression plot
#' data("platelet")
#' fit <- mcreg(
#'   x = platelet$Comparative, y = platelet$Candidate,
#'   method.reg = "Deming", method.ci = "jackknife"
#' )
#' autoplot(fit)
#'
#' # Only present the regression line and alter the color and shape.
#' autoplot(fit,
#'   identity = FALSE,
#'   reg.params = list(col = "grey", linetype = "dashed"),
#'   legend.title = FALSE,
#'   legend.digits = 4
#' )
setMethod(
  f = "autoplot",
  signature = c("MCResult"),
  definition = function(object,
                        color = "black",
                        fill = "lightgray",
                        size = 1.5,
                        shape = 21,
                        jitter = FALSE,
                        identity = TRUE,
                        identity.params = list(col = "gray", linetype = "dashed"),
                        reg = TRUE,
                        reg.params = list(col = "blue", linetype = "solid"),
                        equal.axis = FALSE,
                        legend.title = TRUE,
                        legend.digits = 2,
                        x.nbreak = NULL,
                        y.nbreak = NULL,
                        x.title = NULL,
                        y.title = NULL,
                        main.title = NULL) {
    assert_class(object, "MCResult")
    assert_character(color)
    assert_character(fill)
    assert_number(size)
    assert_int(shape)
    assert_logical(jitter)
    assert_logical(reg)
    assert_logical(identity)
    assert_logical(equal.axis)
    assert_logical(legend.title)
    assert_int(legend.digits)
    assert_int(x.nbreak, null.ok = TRUE)
    assert_int(x.nbreak, null.ok = TRUE)
    assert_subset(names(identity.params), c("col", "linetype"))
    assert_subset(names(reg.params), c("col", "linetype"))

    df <- object@data %>% na.omit()
    xrange <- range(df[["x"]])
    yrange <- range(df[["y"]])

    slope <- formatC(object@glob.coef[2], format = "f", legend.digits)
    intercept <- formatC(object@glob.coef[1], format = "f", legend.digits)
    fm_text <- paste0("Y = ", slope, " * X + ", intercept)

    p <- ggplot(data = df, aes(x = .data$x, y = .data$y))

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

    if (reg) {
      p <- p +
        geom_abline(
          aes(
            slope = as.numeric(slope), intercept = as.numeric(intercept),
            linetype = fm_text, color = fm_text
          ),
          key_glyph = draw_key_path
        )
    }

    if (identity) {
      p <- p +
        geom_abline(
          aes(
            slope = 1, intercept = 0,
            linetype = "Identity", color = "Identity"
          ),
          key_glyph = draw_key_path
        )
    }

    legend_title <- paste0(
      object@regmeth, " RegressionFit", " (n=", nrow(object@data), ")"
    )
    shapes <- stats::setNames(
      c(
        ifelse(is.null(reg.params[["linetype"]]), 1, reg.params[["linetype"]]),
        ifelse(is.null(identity.params[["linetype"]]), 1, identity.params[["linetype"]])
      ),
      c(fm_text, "Identity")
    )
    cols <- stats::setNames(
      c(
        ifelse(is.null(reg.params[["col"]]), 1, reg.params[["col"]]),
        ifelse(is.null(identity.params[["col"]]), 1, identity.params[["col"]])
      ),
      c(fm_text, "Identity")
    )
    p <- p +
      scale_linetype_manual(
        name = legend_title,
        values = shapes
      ) +
      scale_color_manual(
        name = legend_title,
        values = cols
      )

    p <- p + ggplot2::labs(
      x = object@mnames[1], y = object@mnames[2], title = main.title
    )

    if (equal.axis) {
      max_axis <- max(xrange[2] + diff(xrange) * 0.1, yrange[2] + diff(yrange) * 0.1)
      p <- p +
        scale_x_continuous(
          limits = c(0, max_axis),
          breaks = if (is.null(x.nbreak)) scales::pretty_breaks(6) else waiver(),
          n.breaks = x.nbreak
        ) +
        scale_y_continuous(
          limits = c(0, max_axis),
          breaks = if (is.null(y.nbreak)) scales::pretty_breaks(6) else waiver(),
          n.breaks = y.nbreak
        )
    } else {
      p <- p +
        scale_x_continuous(
          limits = c(0, xrange[2] + diff(xrange) * 0.1),
          breaks = if (is.null(x.nbreak)) scales::pretty_breaks(6) else waiver(),
          n.breaks = x.nbreak
        ) +
        scale_y_continuous(
          limits = c(0, yrange[2] + diff(yrange) * 0.1),
          breaks = if (is.null(y.nbreak)) scales::pretty_breaks(6) else waiver(),
          n.breaks = y.nbreak
        )
    }

    p +
      theme_light() +
      theme(
        legend.position = c("inside"),
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = if (!legend.title) element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12, margin = margin(c(5, 0, 0, 0))),
        axis.title.y = element_text(size = 12, margin = margin(c(0, 5, 0, 0))),
        plot.margin = margin(c(15, 15, 10, 10))
      )
  }
)

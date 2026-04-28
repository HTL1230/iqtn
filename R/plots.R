#' Plot a carbon allowance price series
#'
#' @param data A data frame containing date and price columns.
#' @param date_col Name of the date column.
#' @param price_col Name of the price column.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param line_col Line color.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return The plotted data, invisibly.
#' @export
plot_price_series <- function(
    data,
    date_col = "date",
    price_col = "close",
    main = "Carbon allowance price",
    xlab = "Date",
    ylab = "Price",
    line_col = "#1F6E8C",
    ...) {
  require_columns(data, c(date_col, price_col))
  x <- data[[date_col]]
  y <- data[[price_col]]

  graphics::plot(
    x, y,
    type = "l",
    col = line_col,
    lwd = 2,
    main = main,
    xlab = xlab,
    ylab = ylab,
    xaxt = "n",
    ...
  )
  draw_x_axis(x)
  graphics::grid(col = grDevices::adjustcolor("gray70", alpha.f = 0.35))
  invisible(data.frame(date = x, price = y))
}

#' Plot a loss distribution
#'
#' @param loss Numeric vector of realized losses.
#' @param var Optional VaR threshold to display.
#' @param cvar Optional CVaR threshold to display.
#' @param breaks Histogram breaks.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param fill_col Histogram fill color.
#' @param ... Additional arguments passed to [graphics::hist()].
#'
#' @return Histogram object, invisibly.
#' @export
plot_loss_distribution <- function(
    loss,
    var = NULL,
    cvar = NULL,
    breaks = "Sturges",
    main = "Loss distribution",
    xlab = "Loss",
    fill_col = "#D9E4EC",
    ...) {
  loss <- as.numeric(loss)
  hist_obj <- graphics::hist(
    loss,
    breaks = breaks,
    col = fill_col,
    border = "white",
    main = main,
    xlab = xlab,
    ...
  )
  graphics::grid(col = grDevices::adjustcolor("gray70", alpha.f = 0.35))
  if (!is.null(var)) {
    graphics::abline(v = var, col = "#C43C39", lwd = 2, lty = 2)
  }
  if (!is.null(cvar)) {
    graphics::abline(v = cvar, col = "#7A3E9D", lwd = 2, lty = 3)
  }
  invisible(hist_obj)
}

#' Plot realized losses and VaR forecasts
#'
#' @param data A data frame containing date, loss, and VaR columns.
#' @param date_col Name of the date column.
#' @param loss_col Name of the realized loss column.
#' @param var_col Name of the VaR forecast column.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param legend_position Legend position. Use `"bottom"` to place the legend
#'   below the x-axis, `"topleft"` to place it inside the plotting area, or
#'   `"none"` to suppress it.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return A data frame with exceedance indicators, invisibly.
#' @export
plot_var_backtest <- function(
    data,
    date_col = "date",
    loss_col = "loss",
    var_col = "var",
    main = "VaR backtest",
    xlab = "Date",
    ylab = "Loss",
    legend_position = c("bottom", "topleft", "none"),
    ...) {
  require_columns(data, c(date_col, loss_col, var_col))
  legend_position <- match.arg(legend_position)
  x <- data[[date_col]]
  loss <- as.numeric(data[[loss_col]])
  var_forecast <- as.numeric(data[[var_col]])
  exceed <- loss > var_forecast

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  if (legend_position == "bottom") {
    graphics::par(mar = c(7.5, 4.5, 4, 1) + 0.1, xpd = NA)
  }

  y_range <- range(c(loss, var_forecast), na.rm = TRUE)
  graphics::plot(
    x, loss,
    type = "l",
    col = "#2F4858",
    lwd = 1.6,
    ylim = y_range,
    main = main,
    xlab = xlab,
    ylab = ylab,
    xaxt = "n",
    ...
  )
  draw_x_axis(x)
  graphics::lines(x, var_forecast, col = "#C43C39", lwd = 2)
  graphics::points(x[exceed], loss[exceed], pch = 19, col = "#C43C39")
  graphics::grid(col = grDevices::adjustcolor("gray70", alpha.f = 0.35))
  if (legend_position != "none") {
    legend_labels <- c("Realized loss", "VaR", "Exceedance")
    if (legend_position == "bottom") {
      graphics::legend(
        "bottom",
        inset = c(0, -0.34),
        legend = legend_labels,
        col = c("#2F4858", "#C43C39", "#C43C39"),
        lty = c(1, 1, NA),
        pch = c(NA, NA, 19),
        horiz = TRUE,
        x.intersp = 1.2,
        seg.len = 2.8,
        text.width = max(graphics::strwidth(legend_labels, units = "user")) * 1.35,
        bty = "n",
        xpd = NA
      )
    } else {
      graphics::legend(
        "topleft",
        legend = legend_labels,
        col = c("#2F4858", "#C43C39", "#C43C39"),
        lty = c(1, 1, NA),
        pch = c(NA, NA, 19),
        bty = "n"
      )
    }
  }

  invisible(data.frame(date = x, loss = loss, var = var_forecast, exceedance = exceed))
}

#' Plot warning levels over time
#'
#' @param data A data frame containing date and warning-level columns.
#' @param date_col Name of the date column.
#' @param level_col Name of the warning-level column. Values should be 0, 1, 2,
#'   and 3.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return The plotted warning data, invisibly.
#' @export
plot_warning_timeline <- function(
    data,
    date_col = "date",
    level_col = "warning_level",
    main = "Warning timeline",
    xlab = "Date",
    ylab = "Warning level",
    ...) {
  require_columns(data, c(date_col, level_col))
  x <- data[[date_col]]
  level <- as.integer(data[[level_col]])
  cols <- c("#3A7D44", "#D0A215", "#D66A2A", "#B32134")

  graphics::plot(
    x, level,
    type = "n",
    ylim = c(-0.2, 3.2),
    yaxt = "n",
    main = main,
    xlab = xlab,
    ylab = ylab,
    xaxt = "n",
    ...
  )
  draw_x_axis(x)
  graphics::axis(2, at = 0:3, labels = c("normal", "watch", "warning", "severe"), las = 1)
  graphics::grid(col = grDevices::adjustcolor("gray70", alpha.f = 0.35))
  graphics::points(x, level, pch = 15, col = cols[pmax(pmin(level, 3L), 0L) + 1L])
  graphics::lines(x, level, col = grDevices::adjustcolor("#2F4858", alpha.f = 0.35))

  invisible(data.frame(date = x, warning_level = level))
}

#' Plot a risk score time series
#'
#' @param data A data frame containing date and risk-score columns.
#' @param date_col Name of the date column.
#' @param score_col Name of the risk-score column.
#' @param thresholds Optional numeric thresholds displayed as horizontal lines.
#' @param main Plot title.
#' @param xlab X-axis label.
#' @param ylab Y-axis label.
#' @param ... Additional arguments passed to [graphics::plot()].
#'
#' @return The plotted risk-score data, invisibly.
#' @export
plot_risk_score <- function(
    data,
    date_col = "date",
    score_col = "risk_score",
    thresholds = NULL,
    main = "Risk score",
    xlab = "Date",
    ylab = "Risk score",
    ...) {
  require_columns(data, c(date_col, score_col))
  x <- data[[date_col]]
  score <- as.numeric(data[[score_col]])

  graphics::plot(
    x, score,
    type = "l",
    col = "#2F4858",
    lwd = 1.8,
    main = main,
    xlab = xlab,
    ylab = ylab,
    xaxt = "n",
    ...
  )
  draw_x_axis(x)
  graphics::grid(col = grDevices::adjustcolor("gray70", alpha.f = 0.35))
  if (!is.null(thresholds)) {
    graphics::abline(h = thresholds, col = "#C43C39", lwd = 1.5, lty = 2)
  }
  invisible(data.frame(date = x, risk_score = score))
}

require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

draw_x_axis <- function(x) {
  if (inherits(x, "Date")) {
    ticks <- pretty(x)
    graphics::axis(1, at = ticks, labels = format(ticks, "%Y-%m"))
  } else {
    graphics::axis(1)
  }
}

#' Construct carbon allowance market features
#'
#' @param data A data frame with columns `date`, `open`, `high`, `low`, `close`,
#'   `volume`, and `value`.
#' @param volatility_windows Integer vector of rolling windows used for return
#'   volatility and maximum-loss features.
#'
#' @return A data frame containing the original data and derived market-state
#'   features.
#' @export
carbon_features <- function(data, volatility_windows = c(5L, 10L, 20L)) {
  required <- c("date", "open", "high", "low", "close", "volume", "value")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop("Missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  x <- data[order(data$date), , drop = FALSE]
  close <- as.numeric(x$close)
  high <- as.numeric(x$high)
  low <- as.numeric(x$low)
  volume <- as.numeric(x$volume)
  value <- as.numeric(x$value)

  ret <- c(NA_real_, 100 * diff(log(close)))
  x$return <- ret
  x$range <- 100 * log(high / low)
  x$log_volume <- log1p(volume)
  x$log_value <- log1p(value)
  x$absolute_return <- abs(ret)
  x$squared_return <- ret^2
  x$illiquidity <- 1e6 * abs(ret) / (value + 1)

  for (window in volatility_windows) {
    suffix <- as.character(window)
    x[[paste0("volatility_", suffix)]] <- rolling_sd(ret, window)
    x[[paste0("max_loss_", suffix)]] <- rolling_max(-ret, window)
  }

  month <- as.integer(format(as.Date(x$date), "%m"))
  x$year_end_pressure <- as.integer(month %in% c(11L, 12L))
  x
}

rolling_sd <- function(x, window) {
  out <- rep(NA_real_, length(x))
  if (length(x) < window) {
    return(out)
  }
  for (i in seq.int(window, length(x))) {
    values <- x[(i - window + 1L):i]
    out[i] <- stats::sd(values, na.rm = FALSE)
  }
  out
}

rolling_max <- function(x, window) {
  out <- rep(NA_real_, length(x))
  if (length(x) < window) {
    return(out)
  }
  for (i in seq.int(window, length(x))) {
    values <- x[(i - window + 1L):i]
    out[i] <- max(values, na.rm = FALSE)
  }
  out
}

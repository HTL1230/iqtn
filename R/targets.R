#' Future loss from a price series
#'
#' @param data A data frame containing a price column.
#' @param horizon Forecast horizon, measured in rows.
#' @param price_col Name of the price column.
#' @param scale Multiplicative scale for log returns. The default, `100`,
#'   returns percentage log losses.
#'
#' @return A numeric vector with the same length as `data`. The last `horizon`
#'   observations are `NA` because their future price is unavailable.
#' @export
future_loss <- function(data, horizon = 1L, price_col = "close", scale = 100) {
  require_columns(data, price_col)
  horizon <- validate_horizon(horizon)
  price <- as.numeric(data[[price_col]])
  n <- length(price)
  out <- rep(NA_real_, n)
  if (n <= horizon) {
    return(out)
  }
  idx <- seq_len(n - horizon)
  out[idx] <- -scale * log(price[idx + horizon] / price[idx])
  out
}

#' Add future-loss columns for one or more horizons
#'
#' @param data A data frame containing a price column.
#' @param horizons Forecast horizons, measured in rows.
#' @param price_col Name of the price column.
#' @param scale Multiplicative scale for log returns.
#' @param prefix Prefix used for generated loss columns.
#'
#' @return The input data frame with additional future-loss columns.
#' @export
add_future_loss <- function(
    data,
    horizons = c(1L, 5L, 10L),
    price_col = "close",
    scale = 100,
    prefix = "loss_h") {
  out <- data
  for (horizon in horizons) {
    horizon <- validate_horizon(horizon)
    out[[paste0(prefix, horizon)]] <- future_loss(out, horizon, price_col, scale)
  }
  out
}

validate_horizon <- function(horizon) {
  if (!is.numeric(horizon) || length(horizon) != 1L || is.na(horizon) || horizon < 1L) {
    stop("`horizon` must be a positive integer.", call. = FALSE)
  }
  as.integer(horizon)
}

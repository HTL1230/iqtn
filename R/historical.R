#' Rolling historical-simulation VaR
#'
#' @param loss Numeric vector of realized losses.
#' @param alpha VaR confidence level in `(0, 1)`.
#' @param window Rolling lookback window.
#' @param min_obs Minimum number of non-missing observations required.
#'
#' @return A numeric vector of VaR forecasts. Forecasts use only observations
#'   before the forecast origin.
#' @export
historical_var <- function(loss, alpha = 0.95, window = 120L, min_obs = window) {
  validate_alpha(alpha)
  rolling_tail_forecast(loss, alpha, window, min_obs, statistic = "var")
}

#' Rolling historical-simulation CVaR
#'
#' @param loss Numeric vector of realized losses.
#' @param alpha VaR confidence level in `(0, 1)`.
#' @param window Rolling lookback window.
#' @param min_obs Minimum number of non-missing observations required.
#'
#' @return A numeric vector of CVaR forecasts. Forecasts use only observations
#'   before the forecast origin.
#' @export
historical_cvar <- function(loss, alpha = 0.95, window = 120L, min_obs = window) {
  validate_alpha(alpha)
  rolling_tail_forecast(loss, alpha, window, min_obs, statistic = "cvar")
}

rolling_tail_forecast <- function(loss, alpha, window, min_obs, statistic) {
  loss <- as.numeric(loss)
  window <- validate_horizon(window)
  min_obs <- validate_horizon(min_obs)
  if (min_obs > window) {
    stop("`min_obs` cannot be larger than `window`.", call. = FALSE)
  }

  n <- length(loss)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    end <- i - 1L
    start <- max(1L, i - window)
    if (end < start) {
      next
    }
    values <- loss[start:end]
    values <- values[is.finite(values)]
    if (length(values) < min_obs) {
      next
    }
    var <- as.numeric(stats::quantile(values, probs = alpha, names = FALSE, type = 8))
    if (statistic == "var") {
      out[i] <- var
    } else {
      tail_values <- values[values >= var]
      out[i] <- mean(tail_values)
    }
  }
  out
}

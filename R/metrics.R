#' Quantile pinball loss
#'
#' @param y Numeric vector of realized values.
#' @param q Numeric vector of quantile forecasts.
#' @param alpha Quantile level in `(0, 1)`.
#'
#' @return Mean pinball loss.
#' @export
pinball_loss <- function(y, q, alpha) {
  validate_alpha(alpha)
  if (length(y) != length(q)) {
    stop("`y` and `q` must have the same length.", call. = FALSE)
  }
  u <- y - q
  mean(u * (alpha - as.numeric(u < 0)), na.rm = TRUE)
}

#' VaR exceedance rate
#'
#' @param loss Numeric vector of realized losses.
#' @param var Numeric vector of VaR forecasts.
#'
#' @return Mean exceedance indicator.
#' @export
var_exceedance_rate <- function(loss, var) {
  if (length(loss) != length(var)) {
    stop("`loss` and `var` must have the same length.", call. = FALSE)
  }
  mean(loss > var, na.rm = TRUE)
}

#' Approximate CVaR from predicted quantiles
#'
#' @param quantiles Numeric vector of predicted quantiles.
#' @param levels Numeric vector of quantile levels corresponding to `quantiles`.
#' @param alpha Lower tail-probability bound for CVaR integration.
#'
#' @return A scalar CVaR approximation.
#' @export
cvar_from_quantiles <- function(quantiles, levels, alpha = 0.95) {
  validate_alpha(alpha)
  if (length(quantiles) != length(levels)) {
    stop("`quantiles` and `levels` must have the same length.", call. = FALSE)
  }
  keep <- levels >= alpha
  if (sum(keep) < 2L) {
    stop("At least two quantile levels at or above `alpha` are required.", call. = FALSE)
  }

  ord <- order(levels[keep])
  u <- levels[keep][ord]
  q <- quantiles[keep][ord]
  area <- sum(diff(u) * (utils::head(q, -1L) + utils::tail(q, -1L)) / 2)
  area / (max(u) - min(u))
}

validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single numeric value in (0, 1).", call. = FALSE)
  }
  invisible(TRUE)
}

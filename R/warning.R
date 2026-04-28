#' Construct early-warning levels from VaR and CVaR forecasts
#'
#' @param var Numeric vector of VaR forecasts.
#' @param cvar Numeric vector of CVaR forecasts.
#' @param train_index Integer or logical index identifying training observations.
#' @param weights Numeric vector of length two for VaR and CVaR score weights.
#' @param probs Training-period score thresholds.
#'
#' @return A data frame with risk scores and warning levels.
#' @export
early_warning_levels <- function(
    var,
    cvar,
    train_index,
    weights = c(0.5, 0.5),
    probs = c(0.50, 0.75, 0.90)) {
  if (length(var) != length(cvar)) {
    stop("`var` and `cvar` must have the same length.", call. = FALSE)
  }
  if (length(weights) != 2L || any(weights < 0) || sum(weights) <= 0) {
    stop("`weights` must contain two non-negative values with positive sum.", call. = FALSE)
  }
  weights <- weights / sum(weights)

  train_index <- normalize_train_index(train_index, length(var))
  var_z <- standardize_by_train(var, train_index)
  cvar_z <- standardize_by_train(cvar, train_index)
  score <- weights[1L] * var_z + weights[2L] * cvar_z
  cuts <- stats::quantile(score[train_index], probs = probs, na.rm = TRUE, names = FALSE)

  level <- rep(0L, length(score))
  level[score > cuts[1L]] <- 1L
  level[score > cuts[2L]] <- 2L
  level[score > cuts[3L]] <- 3L

  data.frame(
    risk_score = score,
    warning_level = level,
    warning_label = factor(
      level,
      levels = 0:3,
      labels = c("normal", "watch", "warning", "severe")
    )
  )
}

normalize_train_index <- function(train_index, n) {
  if (is.logical(train_index)) {
    if (length(train_index) != n) {
      stop("Logical `train_index` must have the same length as `var`.", call. = FALSE)
    }
    return(train_index)
  }
  out <- rep(FALSE, n)
  out[train_index] <- TRUE
  out
}

standardize_by_train <- function(x, train_index) {
  center <- mean(x[train_index], na.rm = TRUE)
  scale <- stats::sd(x[train_index], na.rm = TRUE)
  if (!is.finite(scale) || scale == 0) {
    stop("Training-period standard deviation must be positive.", call. = FALSE)
  }
  (x - center) / scale
}

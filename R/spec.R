#' Create an IQTN model specification
#'
#' @param lookback Number of trading days in the input sequence.
#' @param horizons Forecast horizons.
#' @param quantile_levels Quantile levels estimated by the model.
#' @param hidden_dim Hidden dimension for the temporal encoder.
#' @param kernel_size Temporal convolution kernel size.
#' @param dilations Dilation factors for temporal convolution blocks.
#' @param dropout Dropout probability.
#'
#' @return A list describing the model specification.
#' @export
iqtn_spec <- function(
    lookback = 20L,
    horizons = c(1L, 5L, 10L),
    quantile_levels = c(0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975, 0.99),
    hidden_dim = 64L,
    kernel_size = 3L,
    dilations = c(1L, 2L, 4L),
    dropout = 0.20) {
  if (lookback < 1L) {
    stop("`lookback` must be positive.", call. = FALSE)
  }
  if (any(quantile_levels <= 0 | quantile_levels >= 1)) {
    stop("All `quantile_levels` must be in (0, 1).", call. = FALSE)
  }
  if (is.unsorted(quantile_levels, strictly = TRUE)) {
    stop("`quantile_levels` must be strictly increasing.", call. = FALSE)
  }
  if (dropout < 0 || dropout >= 1) {
    stop("`dropout` must be in [0, 1).", call. = FALSE)
  }

  structure(
    list(
      lookback = as.integer(lookback),
      horizons = as.integer(horizons),
      quantile_levels = quantile_levels,
      hidden_dim = as.integer(hidden_dim),
      kernel_size = as.integer(kernel_size),
      dilations = as.integer(dilations),
      dropout = dropout
    ),
    class = "iqtn_spec"
  )
}

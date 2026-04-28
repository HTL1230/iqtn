#' Demo carbon allowance market data
#'
#' A ten-year simulated daily OHLCV data set for demonstrating the package API.
#' The data are not real market observations and should not be used for
#' empirical analysis.
#'
#' @format A data frame with 2609 rows and 7 columns:
#' \describe{
#'   \item{date}{Trading date.}
#'   \item{open}{Opening price.}
#'   \item{high}{Highest price.}
#'   \item{low}{Lowest price.}
#'   \item{close}{Closing price.}
#'   \item{volume}{Trading volume.}
#'   \item{value}{Trading value.}
#' }
#'
#' @examples
#' data(demo_carbon_market)
#' head(demo_carbon_market)
"demo_carbon_market"

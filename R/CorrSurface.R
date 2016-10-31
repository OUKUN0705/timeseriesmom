#' CorrSurface
#'
#' The function plot
#'
#' @param price an object of time serise representing a price series.
#' @param lookback_seq a sequence of lookback length.
#' @param holddays_seq a sequence of holddays length.
#' @param Sign whether the returns or signs of returns are used to calculate
#'   correlation, default is FALSE.
#' @param return_method method of calculating returns.
#'
#' @return a 3_D plot of correlation coefficient on lookback period and holding
#'   period.
#' @export
#'
#' @examples
#' lookback_seq <- seq(from = 1, to = 100, by = 1)
#' holddays_seq <- seq(from = 1, to = 100, by = 1)
#' getSymbols("^GSPC", from = 2010, to = Sys.Date())
#' price <- GSPC[, 6]
#' CorrSurface(price, lookback_seq = lookback_seq, holddays_seq = holddays_seq,
#' Sign = TRUE, return_method = "log")
CorrSurface <- function(price, lookback_seq, holddays_seq, Sign = FALSE,
                        return_method = c("arithmetic", "log")) {

  ##################### check input arguments ##################
  price <- as.xts(price)

  if (sum(class(price) %in% c("zoo", "xts")) == 0)
    stop("Please provide with time series object")

  if (missing(lookback_seq))
    stop("Need to specify lookback sequency.")
  if (any(lookback_seq <= 0) | any(lookback_seq != lookback_seq))
    stop("lookback should be positive integer.")

  if (missing(holddays_seq))
    stop("Need to specify holddays sequency.")
  if (any(holddays_seq <= 0) | any(holddays_seq != holddays_seq))
    stop("holddays should be positive integer.")

  if (!is.logical(Sign))
    stop("Sign should be logical variable.")

  if (!return_method %in% c("arithmetic", "log"))
    stop("return calculation method can only be 'arithmetic' or 'log'")

  #################################
  result <- NULL
  for (l in lookback_seq) {
    for (h in holddays_seq) {
      result <- rbind(result, LookbackHoldCorr(price, lookback = l, holddays = h,
                                               Sign = Sign,
                                               return_method = return_method))
    }
  }

  rho_matrix <- matrix(result[, 3], byrow = TRUE, ncol = length(holddays_seq))

  plot_ly(z = rho_matrix, x = lookback_seq, y = holddays_seq) %>%
    add_surface()
}





#' LookbackHoldCorr
#'
#' The function applies discrete moving window to a price series. Each window is
#' divided into lookback period and holding period. The returns in the lookback
#' period and holding period are calculated with each window. The correlation
#' between the associated p value on a returns in the lookback period and
#' holding period discrete rolling window basis. The rolling step size is the
#' minimum of the lookback period and holding period.
#'
#' @param price an object of time serise representing a price series.
#' @param lookback length of lookback period.
#' @param holddays length of holding period.
#' @param Sign whether the returns or signs of returns are used to calculate
#'   correlation, default is TRUE.
#' @param return_method method of calculating returns.
#'
#' @return a 1 by 4 matrix with lookback period, holding period, return
#'   correlation estimate and p value.
#' @export
#'
#' @examples
#' getSymbols("^GSPC", from = 2010, to = Sys.Date())
#' price <- GSPC[, 6]
#' LookbackHoldCorr(price, 200, 20, TRUE, "log")
#' LookbackHoldCorr(price, 200, 20, FALSE, "log")
#' LookbackHoldCorr(price, 200, 20, TRUE, "arithmetic")
#' LookbackHoldCorr(price, 200, 20, FALSE, "arithmetic")
LookbackHoldCorr <- function(price, lookback, holddays, Sign = TRUE,
                               return_method = c("arithmetic", "log")) {
  ##################### check input arguments ##################
  price <- as.xts(price)

  if (all(class(price) %in% c("zoo", "xts")) != TRUE)
    stop("Please provide with time series object")

  if (missing(lookback))
    stop("Need to specify lookback.")
  if (lookback <= 0 | round(lookback) != lookback)
    stop("lookback should be positive integer.")

  if (missing(holddays))
    stop("Need to specify holddays.")
  if (holddays <= 0 | round(holddays) != holddays)
    stop("holddays should be positive integer.")

  if (!is.logical(Sign))
    stop("Sign should be logical variable.")

  if (!return_method %in% c("arithmetic", "log"))
    stop("return calculation method can only be 'arithmetic' or 'log'")

  ######################
  # lookback period and holding period might be different at lengths.
  # discrete rolling based on the shorter of lookback period and holding
  # period requires triming the first few obserations.
  price   <- checkData(price)
  shorter <- min(lookback, holddays)
  longer  <- max(lookback, holddays)
  rm      <- (dim(price)[1] - longer) %% shorter
  if (rm == 0) {
    price <- price
  } else {
    price   <- price[-(1:(rm - 1))]
  }


  # merge price series of t-LB, t and t+HD
  # get rid of the NAs in the first LB entries and last HD entries
  LB_Cu_HP <- na.omit(merge(lag(price, lookback), price, lag(price, -holddays)))
  dim(LB_Cu_HP)

  names(LB_Cu_HP) <- c(paste("lag", lookback, sep = ""), "current",
                       paste("lead", holddays, sep = ""))

  # get entries of every k = shorter periods for discrete rolling
  LB_Cu_HP <- LB_Cu_HP[seq(from = 1, by = shorter, to = dim(LB_Cu_HP)[1])]

  # calculate lookback period return and holding period return
  if (return_method == "arithmetic") {
    # arithmetic return.
    ret_pair <- merge(LB_Cu_HP[, 2]/LB_Cu_HP[, 1] - 1,
                      LB_Cu_HP[, 3]/LB_Cu_HP[, 2] - 1)
  } else {
    # log return.
    ret_pair <- merge(log(LB_Cu_HP[, 2]/LB_Cu_HP[, 1]),
                      log(LB_Cu_HP[, 3]/LB_Cu_HP[, 2]))
  }
  names(ret_pair) <- c("lag return", "lead return")

  # turn the returns into +1 or -1 according to the signs and correlation will
  # be calculated based on signs
  if (Sign) {
    ret_pair <- sign(ret_pair)
  }

  #
  result <- matrix(c(lookback, holddays, NA, NA), nrow = 1)
  colnames(result) <- c('lookback', 'holding', 'rho', 'p-value')
  rownames(result) <- ""

  result[1, 4] <- cor.test(ret_pair[,"lag return"], ret_pair[,"lead return"],
                           method = "pearson")$p.value
  result[1, 3] <- cor.test(ret_pair[,"lag return"], ret_pair[,"lead return"],
                           method = "pearson")$estimate
  #
  return(result)
}

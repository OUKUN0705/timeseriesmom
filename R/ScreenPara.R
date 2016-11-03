#' Screen
#'
#' dfsdfsdfsd
#'
#' @param x a CorrSurface object returned by function CorrSurface()
#' @param rho_min the minimum threshold for rho to screen parameter combination
#' @param p_max the maximum threshold for p value to screen parameter combination
#'
#' @return a object of class ScreenResult
#' @export
#'
#' @examples
ScreenPara <- function(x, rho_min = 0.3, p_max = 0.1) {

  if (class(x) != "CorrSurface") {
    stop("x must be of class 'CorrSurface' ")
  }

  result <- x[['result']]
  # drop = FALSE in case only one row is screened and
  result <- result[result[, 3] >= rho_min & result[, 4] <= p_max, , drop = FALSE]

  if (nrow(result) == 0) {
    cat("There is no match with the thresholds specified")
  }
    ScreenResult <- list(result = result,
                         Sign = x[['Sign']],
                         return_method = x[['return_method']])
    class(ScreenResult) <- "ScreenResult"
    return(ScreenResult)
}

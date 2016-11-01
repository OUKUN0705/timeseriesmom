#' plot.CorrSurface()
#'
#' plot method for object of class CorrSurface.
#'
#' @param x x is a CorrSurface object returned by function CorrSurface()
#'
#' @return plot a 3 D surface of the correlation against lookback and holding
#'   period with plotly
#' @export
#'
#' @examples
plot.CorrSurface <- function(x) {

  if (class(x) != "CorrSurface") {
    stop("x must be of class 'CorrSurface' ")
  }

  holddays_seq <- x$holddays_seq
  lookback_seq <- x$lookback_seq
  result       <- x$result

  rho_matrix <- matrix(result[, 3], byrow = TRUE, ncol = length(holddays_seq))
  rownames(rho_matrix) <- lookback_seq
  colnames(rho_matrix) <- holddays_seq

  plot_ly(z = rho_matrix) %>%
    add_surface() %>%
    layout(title = "Correlation Surface",
           xaxis = list(title = "holdday", showgrid = TRUE),
           yaxis = list(title = "lookback", showgrid = TRUE)
           )
}

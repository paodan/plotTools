#' Plot 2D Summary Values in Hexagonal Bins
#'
#' This function visualizes the summarized values of a third variable (`z`)
#' across a 2D space defined by variables (`x`, `y`) using hexagonal binning.
#' Each hexagon shows the summary statistic of `z` for all points inside that bin.
#'
#' @param data A `data.frame` containing the variables to be plotted.
#' @param x A string giving the name of the column in `data` to be used as the x-axis.
#' @param y A string giving the name of the column in `data` to be used as the y-axis.
#' @param z A string giving the name of the column in `data` to be summarized within bins.
#' @param summaryFun A function (default = `mean`). The summary function applied to `z`
#'   values inside each hexagonal bin. Common choices include `mean`, `median`, `sum`, etc.
#' @param bins Integer (default = 100). The number of bins along each axis
#'   for hexagonal binning.
#' @param low Color for low values (default = `"grey"`).
#' @param mid Color for midpoint values (default = `"white"`).
#' @param high Color for high values (default = `"red"`).
#' @param midpoint Numeric (default = 0). The midpoint for the diverging color scale.
#' @param title Character string giving the plot title (default = the name of `z`).
#'
#' @details
#' The function uses `ggplot2::stat_summary_hex` to compute summary statistics
#' of the variable `z` in each hexagonal bin defined by (`x`, `y`).
#' The color of each hexagon reflects the summarized value of `z`, with colors
#' interpolated between `low`, `mid`, and `high` around the specified `midpoint`.
#'
#' The function applies a custom theme (`theme_Publication()`) for aesthetics.
#' Make sure this theme function is available in your environment
#' (otherwise replace with `theme_classic()` or other `ggplot2` themes).
#'
#' @return A `ggplot` object showing hexagons colored by the summarized values of `z`.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'
#'   set.seed(123)
#'   df <- data.frame(
#'     x = rnorm(5000),
#'     y = rnorm(5000),
#'     z = rnorm(5000)
#'   )
#'   p <- plot2D_SummaryValue(df, x = "x", y = "y", z = "z", summaryFun = mean)
#'   print(p)
#' }
#'
#' @import ggplot2
#'
#' @export
plot2D_SummaryValue = function(data, x, y, z,
                               summaryFun = mean,
                               bins = 100,
                               low = "grey", mid = "white", high = "red", midpoint = 0,
                               title = z){
  stopifnot(is.data.frame(data))
  stopifnot(x %in% colnames(data))
  stopifnot(y %in% colnames(data))
  stopifnot(z %in% colnames(data))

  ggplot(data, aes(x = .data[[x]], y = .data[[y]], z = .data[[z]])) +
    stat_summary_hex(fun = summaryFun, bins = bins) +
    scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = midpoint) +
    theme_Publication()+
    ggtitle(title)
}

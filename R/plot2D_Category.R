#' Plot 2D Categorical Data
#'
#' This function visualizes categorical labels (`z`) on a 2D embedding (`x`, `y`)
#' such as UMAP or t-SNE, using colored scatter plots. It is useful for showing
#' cluster or category annotations across two-dimensional coordinates.
#'
#' @param data A `data.frame` containing the variables to be plotted.
#' @param x A string giving the name of the column in `data` to be used as the x-axis.
#' @param y A string giving the name of the column in `data` to be used as the y-axis.
#' @param z A string giving the name of the column in `data` representing the categories
#'   or groups to color the points by.
#' @param maxN Integer or `NULL` (default = `NULL`). If provided and smaller than
#'   the total number of rows, a random subset of `maxN` rows is sampled for plotting.
#'   Useful for large datasets.
#' @param point_size Numeric (default = `0.1`). Size of points in the scatter plot.
#' @param alpha Numeric (default = `0.05`). Transparency of points in the scatter plot.
#' @param legend_size Numeric (default = `4`). Size of points shown in the legend.
#' @param legend_alpha Numeric (default = `1`). Transparency of points shown in the legend.
#'
#' @details
#' The function plots categorical data (`z`) on a 2D plane defined by (`x`, `y`).
#' If the dataset is too large, a subset of points is randomly sampled according to `maxN`.
#' Legend aesthetics (point size and alpha) can be adjusted separately from the plot
#' via `legend_size` and `legend_alpha`.
#'
#' The function applies a custom theme (`theme_Publication()`) for aesthetics.
#' Make sure this theme function is available in your environment
#' (otherwise replace with a standard `ggplot2` theme such as `theme_classic()`).
#'
#' @return A `ggplot` object showing the scatter plot of categories.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'
#'   set.seed(123)
#'   df <- data.frame(
#'     x = rnorm(2000),
#'     y = rnorm(2000),
#'     cluster = sample(letters[1:4], 2000, replace = TRUE)
#'   )
#'
#'   # Basic categorical plot
#'   p <- plot2D_Category(df, x = "x", y = "y", z = "cluster")
#'   print(p)
#'
#'   # Use only 1000 points to speed up plotting
#'   p2 <- plot2D_Category(df, x = "x", y = "y", z = "cluster", maxN = 1000)
#'   print(p2)
#' }
#'
#' @import ggplot2
#'
#' @export

plot2D_Category = function(data, x, y, z, maxN = NULL,
                           point_size = .1, alpha = 0.05,
                           legend_size = 4, legend_alpha = 1){
  stopifnot(is.data.frame(data))
  stopifnot(x %in% colnames(data))
  stopifnot(y %in% colnames(data))
  stopifnot(z %in% colnames(data))
  if(!is.null(maxN) && is.numeric(maxN) && maxN > 0 && maxN < nrow(data)){
    data = data[sample(1:nrow(data), maxN, replace = FALSE), ]
  }
  ggplot(data, aes(.data[[x]], .data[[y]], color = .data[[z]])) +
    geom_point(size = point_size, alpha = alpha) +
    theme_Publication()+
    guides(color = guide_legend(override.aes = list(size = legend_size, alpha = legend_alpha)))
}

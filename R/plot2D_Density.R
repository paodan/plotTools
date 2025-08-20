#' Plot 2D Density of Points
#'
#' This function visualizes the density of points in a 2D space using kernel density estimation
#' and colors the scatter plot by local density. It is useful for visualizing large datasets
#' where overplotting makes it difficult to see point distributions.
#'
#' @param data A data.frame containing the variables to be plotted.
#' @param x A string giving the name of the column in data to be used as the x-axis.
#' @param y A string giving the name of the column in data to be used as the y-axis.
#' @param maxN Integer (default = 10000). If the dataset has more than maxN rows,
#'   a random subset of rows of size maxN is sampled for plotting. Set to NULL
#'   to plot all rows.
#' @param bins Integer (default = 100). The resolution (number of grid points per axis)
#'   used for kernel density estimation.
#' @param point_size Numeric (default = 0.5). Size of points in the scatter plot.
#' @param low Color for low density values (default = "white").
#' @param high Color for high density values (default = "red").
#' @param title Character string giving the plot title (default = "Density of points").
#'
#' @details
#' The function estimates the local density of points in the 2D plane using MASS::kde2d,
#' then interpolates these densities to the observed point coordinates with
#' fields::interp.surface. The densities are mapped to the color scale in ggplot2.
#' If a column named "density" already exists in data, it is used directly instead
#' of being recalculated (a warning will be issued).
#'
#' @return A ggplot object showing the points colored by their density.
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   df <- data.frame(
#'     x = rnorm(5000),
#'     y = rnorm(5000)
#'   )
#'   p <- plot2D_Density(df, x = "x", y = "y")
#'   print(p)
#' }
#'
#' @import ggplot2
#' @importFrom MASS kde2d
#' @importFrom fields interp.surface
#'
#' @export
plot2D_Density = function(data, x, y, maxN = 10000,
                          bins = 100,
                          point_size = .5,
                          low = "white", high = "red",
                          title = "Density of points"){
  stopifnot(is.data.frame(data))
  stopifnot(x %in% colnames(data))
  stopifnot(y %in% colnames(data))
  if("density" %in% colnames(data)){
    warning("density column is in data, so this density is used for plotting.")
  } else {
    dens <- kde2d(data[[x]], data[[y]], n = bins)
    data$density <- interp.surface(dens, data[, c(x,y)])
  }
  if(!is.null(maxN) && is.numeric(maxN) && maxN > 0 && maxN < nrow(data)){
    data = data[sample(1:nrow(data), maxN, replace = FALSE), ]
  }
  ggplot(data, aes(x = .data[[x]], y = .data[[y]], color = density)) +
    geom_point(size = point_size) +
    scale_color_gradient(low = low, high = high) +
    theme_Publication()+
    ggtitle(title)
}

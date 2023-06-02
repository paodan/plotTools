#' plot boxplot with comparisons between groups
#' @param data the data frame to be used to plot
#' @param x character indicating the variable on x axis
#' @param y character indicating the variable on y axis
#' @param comparisons A list of length-2 vectors. The entries in the vector are
#' either the names of 2 values on the x-axis or the 2 integers that correspond
#' to the index of the groups of interest, to be compared.
#' @param color either the color name, or the variable to be presented in different color
#' @param facets A set of variables or expressions quoted by vars() and defining
#' faceting groups on the rows or columns dimension. The variables can be named
#' (the names are passed to labeller).
#' For compatibility with the classic interface, can also be a formula or character
#' vector. Use either a one sided formula, ~a + b, or a character vector, c("a", "b").
#' @param scales Should scales be fixed ("fixed", the default), free ("free"),
#' or free in one dimension ("free_x", "free_y")?
#' @param palette the color palette to be used for coloring or filling by groups.
#' Allowed values include "grey" for grey color palettes; brewer palettes e.g.
#' "RdBu", "Blues", ...; or custom color palette e.g. c("blue", "red"); and
#' scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet",
#' "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty".
#' @param legend character, one of "none", "right", "left", "top", "bottom".
#' @param add character vector for adding another plot element (e.g.: dot plot or error bars).
#' Allowed values are one or the combination of: "none", "dotplot", "jitter",
#' "boxplot", "point", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range",
#' "median", "median_iqr", "median_hilow", "median_q1q3", "median_mad", "median_range".
#' @param title the title of the figure.
#' @param method a character string indicating which method to be used for
#' comparing means, the default is "t.test". It can also be "wilcox.test".
#' @param file if saving, the figure file name with an image extension,
#' such as ".png", ".pdf", ".tiff", etc.
#' @param width if saving, the width of the figure, the default is 6
#' @param height if saving, the height of the figure, the default is 6
#' @param units  if saving, the unit of the figure, the default is "in"
#' @param dpi if saving, the resolution of the figure, the default is 300
#' @param ... the data frame to be used to plot
#' @import ggplot2
#' @import ggpubr
#' @export
#' @examples
#' \dontrun{
#' compareBoxplot(mtcars, x = "gear", y = "mpg", comparisons = list(c("3","4"), c("3","5")))
#' compareBoxplot(mtcars, x = "gear", y = "mpg", color = "gear", comparisons = list(c("3","4"), c("3","5")))
#' compareBoxplot(iris, x = "Species", y = "Sepal.Length",
#'                comparisons = list(c("setosa", "versicolor"), c("setosa", "virginica")))
#' iris2 = iris
#' iris2$facetVar = sample(c("A", "B"), nrow(iris2), replace = TRUE)
#' compareBoxplot(iris2, x = "Species", y = "Sepal.Length",
#'                comparisons = list(c("setosa", "versicolor"), c("setosa", "virginica")), facets = "facetVar")
#' }
#'
compareBoxplot = function(data, x, y, comparisons, color = "black",
                          facets = NULL, scales = "fixed", palette = "jco", legend = "none", add = "jitter",
                          title = NULL, method = "t.test", file = NULL,
                          width = 6, height = 6, units = "in", dpi = 300, ...){
  g = ggboxplot(data, x = x, y = y,
                color = color, palette = palette, legend = legend,
                add = add) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  g = g +
    stat_compare_means(comparisons = comparisons, method = method, ...)
  if(!is.null(facets)){
    g = g+
      facet_wrap(facets, scales = scales)
  }
  if(!is.null(file)){
    ggsave(file, plot = g, width = width, height = height, units = units, dpi = dpi)
  }
  return(g)
}

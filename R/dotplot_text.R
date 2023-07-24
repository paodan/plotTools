#' plot pathway/GO enrichment results with the texts in the ploting area
#' @param data a data frame from an enrichResult object
#' @param x character, the variable on the x axis
#' @param y character, the variable on the y axis
#' @param color character, the variable for the colors
#' @param size character, the variable for the point sizes
#' @param fontsize_max numeric, maximum fontsize (not a real fontsize, this is a relative number)
#' @param fontsize_scale numeric, scale to adjust fontsize
#' @importFrom enrichplot dotplot
#' @importFrom funcTools theme_Publication
#' @import ggplot2
#' @examples
#' \dontrun{
#' # pathways is an enrichResult object that either clusterProfiler::enrichGO or
#' ReactomePA::enrichPathway function returns.
#' pathways = data.frame()
#' g = dotplot(pathways, showCategory=30)
#' g
#' dotplot_text(pathways, fontsize_max = 30, fontsize_scale = 1)
#' dotplot_text(g$data, fontsize_max = 24, fontsize_scale = .7)
#' dotplot_text(g$data, fontsize_max = 30, fontsize_scale = 1.8)
#' }
#' @export
dotplot_text = function(data, x = "GeneRatio", y = "Description",
                        color = "p.adjust", size = "Count", fontsize_max = 34,
                        fontsize_scale = 1){
  g1 = ggplot(data, aes(.data[[x]], .data[[y]]))+
    scale_color_continuous(low = "red", high = "blue", name = color)+
    ylab(NULL)+
    theme_Publication() +
    theme(axis.text.y = element_blank())

  size0 = pmin(fontsize_max, fontsize_scale*3.8*730/nchar(as.character(data$Description)))
  g1 = g1 +
    geom_point(aes(color = .data[[color]], size = .data[[size]]))+
    geom_text(aes(x = min(.data[[x]]), label = .data[[y]],
                  size = size0),
              hjust = 0, color = "#555555", show.legend = FALSE)
  cat("If the fontsizes are not right, then adjust fontsize_max and fontsize_scale according to the following fontsize:\n")
  cat(size0)
  return(g1)
}




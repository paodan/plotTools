#' Plot a scree plot based on the result returned by prcomp function
#' @param pca The result returned by prcomp function
#' @import reshape2
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' x = matrix(rnorm(10*20), nrow= 20)
#' pca = prcomp(x)
#' screeplot(pca)
#' }
#'
screeplot = function(pca){
  var_explained = pca$sdev^2 / sum(pca$sdev^2)*100
  gVar = melt(data.frame(Cumulative_Variance = cumsum(var_explained),
                         Variance = var_explained,
                         pc = paste0("PC", seq_along(var_explained))),
              id.vars = "pc")
  gVar$pc = factor(gVar$pc, levels = unique(gVar$pc))

  ggplot(gVar, aes(pc, value, color = variable, group = variable))+
    geom_point(size = 2)+
    geom_line()+
    ylab("% of Variance")+
    xlab(NULL)+
    labs(color = NULL)+
    theme_Publication(x_angle = 90, x_hjust = 1)+
    theme(legend.position="bottom")
}

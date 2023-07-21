
#' Plot metadata information on PCA plot
#' @param x n x 2 matrix, the each column indicates one dimension, the rownames(x)
#' must be identical to rownames(meta)
#' @param meta data.frame, the metadata of the samples that is going to show
#' @param colorCol character, which column of the meta is indicated by the color of the points
#' @param shapeCol character, which column of the meta is indicated by the shape of the points
#' @param sizeCol character, which column of the meta is indicated by the size of the points
#' @param showSide logical, whether to plot the distribution of the points projecting
#' to the x and y axis on the top and right side
#' @param xlab label for X axis
#' @param xlab label for Y axis
#' @export
#' @importFrom funcTools theme_Publication
#' @importFrom ggside geom_xsidedensity geom_ysidedensity
#' @examples
#' \dontrun{
#' pca = prcomp(mtcars)
#' set.seed(1)
#' meta = data.frame(group = sample(c("A", "B"), nrow(mtcars), replace = T),
#'                   gear = mtcars$gear,
#'                   vs = mtcars$vs,
#'                   row.names = rownames(mtcars))
#' data = pca$x[,1:2]
#' plot2D(data, meta)
#' plot2D(data, meta, colorCol= "group")
#' plot2D(data, meta, colorCol= "group", showSide = FALSE)
#' plot2D(data, meta, colorCol= "gear", shapeCol = "group")
#' plot2D(data, meta, colorCol= "gear", shapeCol = "group", sizeCol = "vs")
#' }
plot2D = function(x, meta, colorCol = NULL, shapeCol = NULL, sizeCol = NULL,
                  showSide = TRUE, xlab = "X", ylab = "Y"){
  stopifnot(identical(rownames(x), rownames(meta)))
  stopifnot(ncol(x) == 2)
  colnames(x) = c("X", "Y")

  dat = cbind(x, meta)
  # classic plot :
  if(!is.null(colorCol)){
    ae = aes(x=X, y=Y, color=.data[[colorCol]])
    showSide = is.factor(meta[[colorCol]]) | is.character(meta[[colorCol]]) & showSide
  } else {
    # stop("colorCol must be provided")
    ae = aes(x=X, y=Y)
  }

  if(!is.null(shapeCol)){
    ae = aes_add(ae, shape=.data[[shapeCol]])
  }

  if(!is.null(sizeCol)){
    ae = aes_add(ae, size=.data[[sizeCol]])
  }

  p = ggplot(dat, mapping = ae) +
    geom_point()+
    xlab(xlab)+
    ylab(ylab)+
    theme_Publication()
  if(showSide){
    p = p+
      geom_xsidedensity()+
      geom_ysidedensity()
  }
  p
}

#' Project the samples with high dimensional features onto 2D space
#' @param expr the feature matrix or data frame, where each ROW is a FEATURE, such as a gene, and
#' each COLUMN is a SAMPLE
#' @param meta data.frame, the metadata of the samples that is going to show
#'
#' @param type character, one of the following values:
#' \code{c("PCA", "UMAP", "tSNE", "MDS", "isoMDS")}, isoMDS is a nonmetric MDS
#' performed using MASS package. The default is "PCA"
#'
#' @param colorCol character, which column of the meta is indicated by the color of the points
#' @param shapeCol character, which column of the meta is indicated by the shape of the points
#' @param sizeCol character, which column of the meta is indicated by the size of the points
#' @param PCs vector of two integers, indicating which PCs are to be plotted.
#' This is only valid for PCA plot. The default is to plot \code{c(1, 2)} (PC1 and PC2).
#' @param showSide logical, whether to plot the distribution of the points projecting
#' to the x and y axis on the top and right side
#' @importFrom umap umap
#' @importFrom Rtsne Rtsne
#' @importFrom MASS isoMDS
#' @export
#' @examples
#' \dontrun{
#' set.seed(1)
#' head(mtcars)
#' meta = data.frame(group = sample(c("A", "B"), nrow(mtcars), replace = T),
#'                   gear = mtcars$gear,
#'                   vs = mtcars$vs,
#'                   row.names = rownames(mtcars))
#' head(meta)
#'
#' # PCA
#' plotMetaOn2D(t(mtcars), meta, type = "PCA",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              PCs = c(1,2), showSide = TRUE)
#' # PCA (PC1 vs PC3)
#' plotMetaOn2D(t(mtcars), meta, type = "PCA",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              PCs = c(1,3), showSide = TRUE)
#'
#' # UMAP
#' plotMetaOn2D(t(mtcars), meta, type = "UMAP",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              showSide = TRUE)
#'
#' # tSNE
#' plotMetaOn2D(t(mtcars), meta, type = "tSNE",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              showSide = TRUE)
#'
#' # MDS
#' plotMetaOn2D(t(mtcars), meta, type = "MDS",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              showSide = TRUE)
#'
#' # isoMDS
#' plotMetaOn2D(t(mtcars), meta, type = "isoMDS",
#'              colorCol= "gear", shapeCol = "group", sizeCol = "vs",
#'              showSide = TRUE)
#' }
plotMetaOn2D = function(expr, meta, type = c("PCA", "UMAP", "tSNE", "MDS", "isoMDS"),
                        colorCol = NULL, shapeCol = NULL, sizeCol = NULL,
                        PCs = c(1,2), showSide = TRUE){
  stopifnot(ncol(expr) > 2)
  stopifnot(identical(colnames(expr), rownames(meta)))
  expr = as.matrix(expr)
  type = match.arg(type, c("PCA", "UMAP", "tSNE", "MDS", "isoMDS"))
  if(type == "PCA"){
    pca = prcomp(t(expr))
    p = plotMetaOnPCA(pca = pca, meta = meta,
                      colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
                      PCs = PCs, showSide = showSide)
  } else if (type == "UMAP") {
    xy = umap(t(expr))$layout
    rownames(xy) = rownames(meta)
    p = plot2D(x = xy, meta = meta,
               colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
               showSide = showSide, xlab = "UMAP1", ylab = "UMAP2")
  } else if (type == "tSNE") {
    set.seed(1)
    tsne_out = Rtsne(t(expr), pca=FALSE, perplexity=min(10, (nrow(expr) - 1)/4), theta=0.0)
    xy = tsne_out$Y
    rownames(xy) = rownames(meta)
    p = plot2D(x = xy, meta = meta,
               colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
               showSide = showSide, xlab = "tSNE1", ylab = "tSNE2")
  } else if (type == "MDS") {
    fit = cmdscale(dist(t(expr)), eig=TRUE, k=2) # k is the number of dim
    xy = fit$points
    rownames(xy) = rownames(meta)
    pctVar = (fit$eig^2/sum(fit$eig^2, na.rm = TRUE))[1:2] * 100
    p = plot2D(x = xy, meta = meta,
               colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
               showSide = showSide,
               xlab = paste0("MDS Dim1 (", round(pctVar[1],1), "%)"),
               ylab = paste0("MDS Dim2 (", round(pctVar[2],1), "%)"))
  } else if (type == "isoMDS") {
    fit = isoMDS(dist(t(expr)), k=2) # k is the number of dim
    xy = fit$points
    rownames(xy) = rownames(meta)
    p = plot2D(x = xy, meta = meta,
               colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
               showSide = showSide, xlab = "isoMDS Dim1", ylab = "isoMDS Dim2")
  } else {
    stop("Unknown type")
  }

  return(p)
}

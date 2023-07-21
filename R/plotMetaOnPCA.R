#' Plot metadata information on PCA plot
#' @param pca the results that prcomp function returns
#' @param meta data.frame, the metadata of the samples that is going to show
#' @param colorCol character, which column of the meta is indicated by the color of the points
#' @param shapeCol character, which column of the meta is indicated by the shape of the points
#' @param sizeCol character, which column of the meta is indicated by the size of the points
#' @param PCs vector of two integers, indicating which PCs are to be plotted
#' @param showSide logical, whether to plot the distribution of the points projecting
#' to the x and y axis on the top and right side
#' @export
#' @examples
#' \dontrun{
#' pca = prcomp(mtcars)
#' set.seed(1)
#' meta = data.frame(group = sample(c("A", "B"), nrow(mtcars), replace = T),
#'                   gear = mtcars$gear,
#'                   vs = mtcars$vs,
#'                   row.names = rownames(mtcars))
#' plotMetaOnPCA(pca, meta)
#' plotMetaOnPCA(pca, meta, colorCol= "group")
#' plotMetaOnPCA(pca, meta, colorCol= "group", showSide = FALSE)
#' plotMetaOnPCA(pca, meta, colorCol= "gear", shapeCol = "group")
#' plotMetaOnPCA(pca, meta, colorCol= "gear", shapeCol = "group", sizeCol = "vs")
#' }
plotMetaOnPCA = function(pca, meta, colorCol = NULL, shapeCol = NULL, sizeCol = NULL,
                         PCs = c(1,2), showSide = TRUE){
  if(!is.null(shapeCol))
    stopifnot(colorCol %in% colnames(meta))
  if(!is.null(shapeCol))
    stopifnot(colorCol %in% colnames(meta))
  if(!is.null(sizeCol))
    stopifnot(sizeCol %in% colnames(meta))
  stopifnot(length(PCs) == 2 && is.numeric(PCs))

  pctVar = pca$sdev^2/sum(pca$sdev^2) * 100

  dat = pca$x[,PCs]
  colnames(dat) = c("X", "Y")
  #
  plot2D(x = dat, meta = meta,
         colorCol = colorCol, shapeCol = shapeCol, sizeCol = sizeCol,
         showSide = showSide,
         xlab = paste0("PC", PCs[1], " (", round(pctVar[PCs[1]],1), "%)"),
         ylab = paste0("PC", PCs[2], " (", round(pctVar[PCs[2]],1), "%)"))
}



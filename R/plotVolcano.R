#' Volcano plot
#' @param data either data frame or DataFrame or tibble
#' @param log2fcColumn character, which column (name) is log2 fold change
#' @param pvalColumn character, which column (name) is p value
#' @param averageColumn character, which column (name) is average expression
#' @param log2fc numeric, log2 fold change cutoff, default is 0.
#' @param ave numeric, average expression cutoff, default is NULL.
#' @param p numeric, average p value cutoff, default is 0.05
#' @param label one of c("sig", "none", "all", "up", "down"), whether to annotate
#' the genes (points), and which type of genes to be annotated. "sig": all the genes
#' that pass all of the cutoffs; "none": no genes; "all": all of the genes in the
#' plot; "up": up-regulated genes; "down": down-regulated genes.
#' @returns a ggplot object of a volcano plot.
#' @import ggplot2
#' @examples
#' \dontrun{
#' library(S4Vectors)
#'
#'  ## DataFrame (DFrame)
#' data = DataFrame(gene = letters,
#' log2FC = runif(26, -4, 4),
#' p = seq(0.001, 0.8, length = 26),
#' Ave = rnorm(26, mean = 100, sd = 10))
#' plotVolcano(data)
#' plotVolcano(data, label = "all")
#' plotVolcano(data, label = "up")
#' plotVolcano(data, log2fc = 1, label = "up")
#'
#' ## data frame
#' data2 = as.data.frame(data)
#' plotVolcano(data2)
#'
#' ## tibble
#' data3 = as_tibble(data)
#' g = plotVolcano(data3)
#' g
#'
#' # Add Ave as the point size
#' g$mapping = aes_add(aes = g$mapping, size = Ave)
#' g
#'
#' ## data.table
#' data4 = as.data.table(data)
#' plotVolcano(data4)
#' }
#' @import ggplot2
#' @importFrom funcTools theme_Publication scale_colour_Publication
#' @importFrom ggrepel geom_label_repel
#' @export
plotVolcano = function(data, log2fcColumn = "log2FC", pvalColumn = "p", averageColumn = "Ave", sampleColumn = NULL,
                       log2fc = 0, ave = NULL, p = 0.05, label = c("sig", "none", "all", "up", "down")){
  label = match.arg(label)

  if("DFrame" %in% class(data)){
    data = as.data.frame(data)
  }

  if(!is.null(ave)){
    data$sig = c("Non-sig", "Sig")[1+(abs(data[[log2fcColumn]]) > log2fc & data[[averageColumn]] > ave & data$p < p)]
  } else {
    data$sig = c("Non-sig", "Sig")[1+(abs(data[[log2fcColumn]]) > log2fc & data[[pvalColumn]] < p)]
  }

  data$upDownSig = paste0(data$sig, "_", c("Down", "Up")[1+(data[[log2fcColumn]] > log2fc)])
  if(is.null(sampleColumn)){
    data$gene = rownames(data)
  } else {
    data$gene = data[[sampleColumn]]
  }

  data$negLogP = -log10(data[[pvalColumn]])
  g = ggplot(data, aes(.data[[log2fcColumn]], negLogP, color = upDownSig))+
    # g = ggplot(data, aes(vars(log2fcColumn), -log10(p), color = upDownSig))+
    geom_point(show.legend = FALSE)+
    theme_Publication()+
    scale_colour_Publication(colourColor = c("grey", "grey", "blue", "red"))+
    geom_vline(xintercept = c(-log2fc, log2fc), linetype=3)+
    geom_hline(yintercept = -log10(p), linetype=3)+
    ylab("-log10(P)")
  if(label == "all"){
    g = g + ggrepel::geom_label_repel(aes(label = gene),
                                      show.legend = FALSE)
  } else if(label == "sig") {
    g = g + ggrepel::geom_label_repel(aes(label = gene),
                                      data = subset(data, sig == "Sig"),
                                      show.legend = FALSE)
  } else if(label == "up") {
    print("up")
    g = g + ggrepel::geom_label_repel(aes(label = gene),
                                      data = subset(data, upDownSig == "Sig_Up"),
                                      show.legend = FALSE)
  } else if(label == "down") {
    g = g + ggrepel::geom_label_repel(aes(label = gene),
                                      data = subset(data, upDownSig == "Sig_Down"),
                                      show.legend = FALSE)
  }

  return(g)
}

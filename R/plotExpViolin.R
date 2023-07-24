#' Violin plot for gene expression data and colored by groups
#' @param data a data frame of gene expression data, where each row is a gene and each column is a sample
#' @param genes character vector, a list of genes to plot
#' @param annotation a data frame of mapping gene names, the row name must be the same type of IDs as of `genes`
#' @param pheno_data a data frame of sample information
#' @param external_gene_name the new gene ID column name in annotation, default is "external_gene_name".
#' @param ylab character, the label of y axis
#' @param phenocols character vector, the column names of pheno_data
#' @param outname character, output folder
#' @param namedColors named color vector
#' @param width figure width in inches
#' @param height figure height in inches
#' @param nameedLabels named labels for x axis ticks
#'
#' @import funcTools
#' @import ggplot2
#' @importFrom ggpubr ggarrange annotate_figure
#' @return a (invisible) list of ggplot objects named by the `genes`
#' @examples
#' \dontrun{
#'
#' expr = createExprData(nGene = 100, nSample = 15, min = 5, max = 10)
#' genes = c("G001", "G009", "G089")
#' annotation = data.frame(external_gene_name = paste0("genename", 1:nrow(expr)),
#'                         row.names = rownames(expr))
#' pheno_data = data.frame(group = rep(c("A", "B", "C"), each = 5))
#' ylab = "VSD"
#' phenocols = "group"
#' namedColors = c(A = "red", B = "purple", C = "#99335555")
#' namedLabels = c(A = "GroupA", B = "Group B", C = "Group\nC")
#'
#' plotExpViolin(expr, genes, annotation, pheno_data, "external_gene_name", ylab,
#'               phenocols, outname = "./expression_violin",
#'               namedColors, width = 4, height = 4, namedLabels, assemble = TRUE)
#' }
#'
#' @export
plotExpViolin <- function(data, genes, annotation,
                          pheno_data, external_gene_name="external_gene_name",
                          ylab = "norm counts", phenocols,
                          outname = "./expression_violin", namedColors,
                          width = 4, height = 4,
                          namedLabels = NULL, assemble = FALSE){

  selected_genes <- data[rownames(data) %in% genes,]
  rownames(selected_genes) <- sub("\\.[0-9]*$", "", rownames(selected_genes))

  selected_genes <- data.frame(t(selected_genes))
  selected_genes <- cbind(selected_genes, pheno_data[,phenocols, drop=F]) #, by= "row.names"

  plot_gene_data <- reshape2::melt(selected_genes, id.vars = phenocols)


  g <- list()
  dir.create(outname, F, T)
  n = length(genes)
  pb = txtProgressBar(min = 0, max = n, initial = 0)
  for (mi in seq_along(genes)) {
    gene_id = genes[mi]
    setTxtProgressBar(pb, mi)
    my_data <- plot_gene_data[plot_gene_data$variable == gene_id,]

    colnames(my_data) <- c("condition", "Gene","Expression")
    #print(gene_id)


    if(is.null(namedLabels)){
      namedLabels = setNames(unique(my_data$condition), unique(my_data$condition))
    }
    geneName = annotation[gene_id, external_gene_name][[1]]

    g[[gene_id]] <- ggplot(my_data, aes(x = condition, y = Expression, fill = condition)) +
      geom_violin(trim=FALSE, show.legend = FALSE) +
      geom_jitter(shape = 21, width = .1, size = 2, show.legend = FALSE) +
      labs(x=NULL, y=ylab) +
      ggtitle(geneName) +
      scale_fill_manual(values = namedColors)+
      scale_x_discrete(labels = namedLabels)+
      funcTools::theme_Publication()
    figFile = paste0(outname, "/", geneName, "__", gene_id, ".pdf")
    ggsave(figFile, g[[gene_id]], width = width, height = height)
  }
  close(pb)
  cat("Plotting each panel is done\n")

  if(assemble){
    q <- ggarrange(plotlist = g, ncol=4, nrow = 4, common.legend = TRUE, legend = "bottom")
    q <- annotate_figure(q, left = text_grob(ylab, rot = 90), bottom = text_grob("Condition", hjust = 1, size = 10))
    ggsave(paste0(outname, "/000_all_", basename(outname), ".pdf"), q,
           width = 14, height = 16)
    cat("Plotting assembled figure is done\n")
  }
  return(invisible(g))
}



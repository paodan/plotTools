#' Make a manhattan plot for GWAS result or DNA methylation result
#' @param x data frame containing following information: CHR (chromosome), BP
#' (position of the SNP in the chromosome), P (p value), SNP (SNP name)
#' @param chr A string denoting the column name for the chromosome
#' Defaults to PLINK's "CHR"
#' @param bp A string denoting the column name for the chromosomal position
#' Defaults to PLINK's "BP"
#' @param p A string denoting the column name for the p-value. Defaults to PLINK's "P"
#' @param snp A string denoting the column name for the SNP name (rs number)
#' Defaults to PLINK's "SNP"
#' @param color A character vector indicating which colors to alternate
#' @param suggestiveline Where to draw a "suggestive" line. Default -log10(1e-5)
#' Set to FALSE to disable
#' @param genomewideline Where to draw a "genome-wide sigificant" line
#' Default -log10(5e-8). Set to FALSE to disable
#' @param highlight A character vector of SNPs in your dataset to highlight
#' These SNPs should all be in your dataset
#' @return ggplot object
#' @importFrom ggrepel geom_label_repel
#' @import ggplot2
#' @importFrom gtools mixedsort
#' @importFrom funcTools sortDataframe
#' @examples
#' \dontrun{
#' res = data.frame(SNP = paste0("rs", 1:15000),
#'                  CHR = c(rep(1, 1500), rep_len(c(2:22, "X", "Y"), 13500)),
#'                  P = sample(c(runif(20, 10^-9, 10^-7), runif(15000-10, 10^-7, 1)), 15000))
#' res$BP = unlist(tapply(res$SNP, res$CHR, function(x) 1:length(x)))
#' highlight = res[res$P<5e-08, "SNP"]
#'
#' plotManhattan(res)
#' plotManhattan(res, suggestiveline = FALSE, genomewideline = FALSE)
#' plotManhattan(res, chr = "CHR", bp = "BP", p = "P", snp = "SNP",
#'               color = NULL,
#'               suggestiveline = -log10(1e-05),
#'               genomewideline = -log10(5e-08),
#'               highlight = highlight)
#' }
#' @export
plotManhattan = function(x,
                         chr = "CHR",
                         bp = "BP",
                         p = "P",
                         snp = "SNP",
                         color = c("gray10", "gray60"),
                         suggestiveline = -log10(1e-05),
                         genomewideline = -log10(5e-08),
                         highlight = NULL){
  x[[chr]] = factor(x[[chr]], levels = mixedsort(unique(x[[chr]])))
  gr0 = sortDataframe(x, by = c(chr, bp))
  gr0$idx = 1:nrow(gr0)
  gr0$neglog10p = -log10(gr0[[p]])

  breaks = tapply(gr0$idx, gr0[[chr]], median)
  borders = c(0, tapply(gr0$idx, gr0[[chr]], max))
  g = ggplot(gr0, aes(x = idx, y = neglog10p, color = as.factor(.data[[chr]])))+
    geom_point(show.legend = FALSE)+
    scale_x_continuous(breaks = breaks,
                       minor_breaks = borders, expand = c(.01, .01))+
    theme(panel.grid.major.x = element_blank())+
    xlab("Chromosome")+ylab(expression("-log"[10]*"(P)"))
  if(suggestiveline)
    g = g + geom_hline(yintercept = c(suggestiveline), color = "blue")
  if(genomewideline)
    g = g + geom_hline(yintercept = c(genomewideline), color = "red")
  if(!is.null(color))
    g = g + scale_color_manual(values=rep_len(color, length(breaks)))
  if (!is.null(highlight)) {
    if (any(!(highlight %in% gr0[[snp]])))
      warning("You're trying to highlight the sites that don't exist in your results.")
    x_highlight = gr0[gr0[[snp]] %in% highlight, ]
    g = g + geom_point(data = x_highlight, show.legend = FALSE)+
      geom_label_repel(aes(label = .data[[snp]]),
                       data = x_highlight,
                       show.legend = FALSE)
  }
  g
  return(g)
}

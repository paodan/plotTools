#' Generate an example expression data
#' @param nGene integer, number of genes
#' @param nSample integer, number of genes
#' @param min numeric, minimum value
#' @param max numeric, maximum value
#' @importFrom funcTools formatN
#' @examples
#' \dontrun{
#' createExprData(nGene = 100, nSample = 15, min = 5, max = 10)
#' createExprData(nGene = 200, nSample = 30, min = 5, max = 10)
#' }
#' @export
createExprData = function(nGene = 100, nSample = 15, min = 5, max = 10){
  expr = matrix(runif(nGene * nSample, min, max), nrow = nGene,
                dimnames = list(paste0("G", funcTools::formatN(1:nGene, n = log10(nGene) + 1)),
                                paste0("S", funcTools::formatN(1:nSample, n = log10(nSample) + 1))))
  return(as.data.frame(expr))
}

#' Association between the metadata with the principle components of expression data
#'
#' Perform statistical test (either ANOVA or kruskal test for categorical data, and a
#' linear model for numerical data) to associate the metadata with the principle
#' components of expression data. A matrix of p-values is returned, and the percentage of
#' variance contributed by each principle component are returned.
#'
#' @param expr Expression-like data (genes in rows, samples in columns)
#' @param pd phenodata, (also called metadata)
#' @param Npcs Number of principle components
#' @param factorTestMethod which method is used for testing factor variable
#' @param scale. whether to scale the expr for PCA analysis
#' @examples
#' \dontrun{
#' p = PCA_ANOVA(expr, pd, Npcs = 10, factorTestMethod = "anova", scale. = TRUE)
#' print(p)
#' attr(p, "percent_variance")
#' }
#'
PCA_ANOVA = function (expr, pd, Npcs = NULL,
                      factorTestMethod = c("anova", "kruskal"),
                      scale. = TRUE) {
  factorTestMethod = match.arg(factorTestMethod)
  thdens <- function(Q, sigma2, ns) {
    lambdaMAX <- sigma2 * (1 + 1/Q + 2 * sqrt(1/Q))
    lambdaMIN <- sigma2 * (1 + 1/Q - 2 * sqrt(1/Q))
    delta <- lambdaMAX - lambdaMIN
    roundN <- 3
    step <- round(delta/ns, roundN)
    while (step == 0) {
      roundN <- roundN + 1
      step <- round(delta/ns, roundN)
    }
    lambda.v <- seq(lambdaMIN, lambdaMAX, by = step)
    dens.v <- vector()
    ii <- 1
    for (i in lambda.v) {
      dens.v[ii] <- (Q/(2 * pi * sigma2)) * sqrt((lambdaMAX - i) * (i - lambdaMIN))/i
      ii <- ii + 1
    }
    return(list(min = lambdaMIN, max = lambdaMAX, step = step,
                lambda = lambda.v, dens = dens.v))
  }
  EstDimRMTv2 <- function(data.m) {
    M <- data.m
    for (c in 1:ncol(M)) M[, c] <- (data.m[, c] - mean(data.m[, c]))/sqrt(var(data.m[, c]))
    sigma2 <- var(as.vector(M))
    Q <- nrow(data.m)/ncol(data.m)
    thdens.o <- thdens(Q, sigma2, ncol(data.m))
    C <- 1/nrow(M) * t(M) %*% M
    eigen.o <- eigen(C, symmetric = TRUE)
    estdens.o <- density(eigen.o$values, from = min(eigen.o$values),
                         to = max(eigen.o$values), cut = 0)
    # GenPlot(thdens.o, estdens.o, eigen.o$values)
    intdim <- length(which(eigen.o$values > thdens.o$max))
    return(list(cor = C, dim = intdim, estdens = estdens.o,
                thdens = thdens.o))
  }
  if (length(which(is.na(expr))) > 0)
    message(length(which(is.na(expr))), " NA are detected in your beta Data Set, which may cause fail or uncorrect of SVD analysis. You may want to impute NA with champ.impute() function first.")
  if (is.data.frame(expr)) {
    message("Your beta parameter is data.frame format, changing it to matrix.")
    expr <- as.matrix(expr)
  }
  message("[PCA analysis will be proceed with ", dim(expr)[1],
          " features and ", dim(expr)[2], " samples.]\n")
  message("\n[ PCA_ANOVA() will only check the dimensions between data and pd]\n")
  if (is.null(pd) | class(pd) == "list")
    stop("pd parameter in Data Frame or Matrix is necessary And must contain at least two factors. If your pd is a list, please change its Format.")
  if (class(pd) == "matrix")
    pd <- as.data.frame(pd)
  PhenoTypes.lv_tmp <- pd[, !colnames(pd) %in% c("Sample_Name", "Project", "filenames", "Basename") &
                            apply(pd, 2, function(x) length(unique(x))) != 1]
  PhenoTypes.lv <- PhenoTypes.lv_tmp
  if (!is.null(rownames(pd)))
    rownames(PhenoTypes.lv) <- rownames(pd)
  if (ncol(PhenoTypes.lv) >= 2) {
    message("<< Following Factors in your pd will be analysised: >>")
    sapply(colnames(PhenoTypes.lv_tmp),
           function(x) message("<", x, ">(", class(PhenoTypes.lv[[x]]), "):", paste(unique(PhenoTypes.lv_tmp[, x]), collapse = ", ")))
    message("[PCA_ANOVA have automatically select ALL factors contain at least two different values from your pd(sample_sheet.csv), if you don't want to analysis some of them, please remove them manually from your pd variable then retry PCA_ANOVA().]")
  }
  else {
    stop("You don't have even one factor with at least two value to be analysis. Maybe your factors contains only one value, no variation at all...")
  }
  if (ncol(pd) > ncol(PhenoTypes.lv)) {
    message("\n<< Following Factors in your pd will not be analysis: >>")
    sapply(setdiff(colnames(pd), colnames(PhenoTypes.lv)),
           function(x) message("<", x, ">"))
    message("[Factors are ignored because they only indicate Name or Project, or they contain ONLY ONE value across all Samples.]")
  }
  if (nrow(PhenoTypes.lv) == ncol(expr))
    message("\n<< PhenoTypes.lv generated successfully. >>")
  else stop("Dimension of your pd file is not equal to your expr matrix.")

  if(scale.){
    expr = t(scale(t(expr)))
  }
  pca.o <- prcomp(t(expr), scale. = scale.)
  if(is.null(Npcs)){
    rmt.o_dim <- EstDimRMTv2(expr)$dim
    if (rmt.o_dim > 20)
      Npcs <- 20
    else Npcs <- rmt.o_dim
  }
  pcaPV.m <- matrix(nrow = Npcs, ncol = ncol(PhenoTypes.lv))
  colnames(pcaPV.m) <- colnames(PhenoTypes.lv)
  for (c in 1:Npcs) {
    for (f in 1:ncol(PhenoTypes.lv)) {
      if (class(PhenoTypes.lv[, f]) != "numeric") {
        if(factorTestMethod == "kruskal"){
          pcaPV.m[c, f] <- kruskal.test(pca.o$x[,c] ~ as.factor(PhenoTypes.lv[[f]]))$p.value
        } else if (factorTestMethod == "anova") {
          pcaPV.m[c, f] <- summary(aov(pca.o$x[,c] ~ as.factor(PhenoTypes.lv[[f]])))[[1]][1,5]
        }
      } else {
        pcaPV.m[c, f] <- summary(lm(pca.o$x[,c] ~ PhenoTypes.lv[[f]]))$coeff[2, 4]
      }
    }
  }
  percent_variance  = round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)[1:Npcs]
  attr(pcaPV.m, "percent_variance") = percent_variance
  return(pcaPV.m)
}

#' Plot a heat map for the p values from PCA_ANOVA function
#'
#' @param pvalues a matrix of p values from PCA_ANOVA function
#' @param threshold threshold for p value, above this number will be set to 1,  default 0.05,
#' @param filename file name, default NULL,
#' @param width default 6,
#' @param height default 6,
#' @param units default "in"
#' @export
#' @examples
#' \dontrun{
#' # expr: expression data (each row is a gene, each column is a sample)
#' # pd: phenodata (metadata), each row is a sample, each column is a clinical parameter
#' p = PCA_ANOVA(expr, pd, Npcs = 10, factorTestMethod = "anova", scale. = TRUE)
#' plotPCA_ANOVA_Pvalue(p)
#' }
#'
plotPCA_ANOVA_Pvalue = function(pvalues, threshold = 0.05, filename = NULL,
                                width = 6, height = 6, units = "in"){
  negLogPvalues = -log10(pvalues)
  negLogPvalues[negLogPvalues < -log10(threshold)] = 0

  if(!is.null(attr(pvalues, "percent_variance"))){
    percent_variance = attr(pvalues, "percent_variance")
    rownames(negLogPvalues) = paste0("PC", 1:nrow(negLogPvalues), " (", percent_variance, "%)")
  } else {
    rownames(negLogPvalues) = paste0("PC", 1:nrow(negLogPvalues))
  }
  negLogPvalues = t(negLogPvalues)
  hm = pheatmap(negLogPvalues, cluster_cols = F, cluster_rows = F)
  if(!is.null(filename)){
    plotSave(filename = filename,
             plotCMD = {
               grid.newpage()
               grid.draw(hm)
             },
             width = width, height = height, units = units)
  }
  return(invisible(hm))
}
